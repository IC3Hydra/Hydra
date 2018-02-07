module Main where

import           Control.Monad
import           Data.Char
import qualified Data.Text           as T
import           EVM.Bytecode
import           EVM.GenericInitcode
import qualified EVM.Instrumentation as I
import           EVM.Instrumentation.Metacontract
import           System.Environment
import           System.Exit
import           System.IO

parseHexStringStrict :: String -> Either String [Opcode]
parseHexStringStrict = parse . hexStringToByteString

parseHexStringLenient :: String -> Either String [Opcode]
parseHexStringLenient = parseLenient . hexStringToByteString

printErrorAndExit :: (Show a) => Either a b -> IO b
printErrorAndExit (Left x)  = do hPutStrLn stderr $ show x
                                 exitWith (ExitFailure 1)
printErrorAndExit (Right x) = return x

-- I should make a NPM package
leftpad :: Int -> a -> [a] -> [a]
leftpad n c s = replicate (n - length s) c ++ s

parseAddress :: String -> Either String Integer
parseAddress s = do let s' = map toLower $ leftpad 40 '0' $ if "0x" == take 2 s then drop 2 s else s
                    when (not $ all (`elem` "0123456789abcdef") s') $ Left "address contains non-hex digit"
                    let i = bytestringToInteger . hexStringToByteString $ s'
                    when (not $ 0 <= i && i < 2^256) $ Left "address must be in range [0..2^256["
                    return i


-- TODO(lorenzb): Fix help text
run :: [String] -> IO ()
run ["disasmStrict", contract] = either (hPutStrLn stderr) (putStrLn . unlines . map show) . parseHexStringStrict $ contract
run ["disasmLenient", contract] = either (hPutStrLn stderr) (putStrLn . unlines . map show) . parseHexStringLenient $ contract
run ("metacontract" : head1 : heads) = do headaddrs <- sequence $ map (printErrorAndExit . parseAddress) (head1:heads)
                                          let mc = metacontract headaddrs
                                          putStrLn . byteStringToHexString . assemble $ genericInitcode ++ mc
run ["1sthead", mc, contract] = do mcaddr <- printErrorAndExit $ parseAddress mc
                                   parsed <- printErrorAndExit $ parseHexStringLenient contract
                                   instrumented <- printErrorAndExit $ I.instrumentFirst mcaddr parsed
                                   putStrLn . byteStringToHexString . assemble $ genericInitcode ++ instrumented
run ["nthhead", mc, contract] = do mcaddr <- printErrorAndExit $ parseAddress mc
                                   parsed <- printErrorAndExit $ parseHexStringLenient contract
                                   instrumented <- printErrorAndExit $ I.instrumentNth mcaddr parsed
                                   putStrLn . byteStringToHexString . assemble $ genericInitcode ++ instrumented
run _ = do hPutStrLn stderr $ unlines [ "Invalid option. Usage:                                                     "
                                      , "                                                                           "
                                      , "disasm <bytecode>                                                          "
                                      , "    disassembles the given bytecode                                        "
                                      , "                                                                           "
                                      , "instrument <metacontract> <bytecode>                                       "
                                      , "    instruments the given bytecode, adds initcode and prints to stdout     "
                                      ]
           exitWith (ExitFailure 1)
-- run ["instrument", mc, contract] = do mcaddr <- printErrorAndExit $ parseAddress mc
--                                       parsed <- printErrorAndExit $ parseHexString contract
--                                       instrumented <- printErrorAndExit $ I.instrument mcaddr parsed
--                                       putStrLn . byteStringToHexString . assemble $ genericInitcode ++ instrumented
-- run _ = do hPutStrLn stderr $ unlines [ "Invalid option. Usage:                                                     "
--                                       , "                                                                           "
--                                       , "disasm <bytecode>                                                          "
--                                       , "    disassembles the given bytecode                                        "
--                                       , "                                                                           "
--                                       , "instrument <metacontract> <bytecode>                                       "
--                                       , "    instruments the given bytecode, adds initcode and prints to stdout     "
--                                       ]
--            exitWith (ExitFailure 1)

main :: IO ()
main = getArgs >>= run

-- main :: IO ()
-- main = putStrLn $ show $ I.instrument 0xc0ffee =<< parseHexString "6060604052361561008c576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff168063095ea7b31461008e57806318160ddd146100e557806323b872dd1461010b5780633bed33ce1461018157806370a08231146101b957806398ea5fca14610203578063a9059cbb1461020d578063dd62ed3e14610264575bfe5b341561009657fe5b6100cb600480803573ffffffffffffffffffffffffffffffffffffffff169060200190919080359060200190919050506102cd565b604051808215151515815260200191505060405180910390f35b34156100ed57fe5b6100f56103c6565b6040518082815260200191505060405180910390f35b341561011357fe5b610167600480803573ffffffffffffffffffffffffffffffffffffffff1690602001909190803573ffffffffffffffffffffffffffffffffffffffff169060200190919080359060200190919050506103e6565b604051808215151515815260200191505060405180910390f35b341561018957fe5b61019f600480803590602001909190505061052a565b604051808215151515815260200191505060405180910390f35b34156101c157fe5b6101ed600480803573ffffffffffffffffffffffffffffffffffffffff16906020019091905050610667565b6040518082815260200191505060405180910390f35b61020b6106b1565b005b341561021557fe5b61024a600480803573ffffffffffffffffffffffffffffffffffffffff1690602001909190803590602001909190505061074f565b604051808215151515815260200191505060405180910390f35b341561026c57fe5b6102b7600480803573ffffffffffffffffffffffffffffffffffffffff1690602001909190803573ffffffffffffffffffffffffffffffffffffffff1690602001909190505061076b565b6040518082815260200191505060405180910390f35b6000600033905082600160008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055508373ffffffffffffffffffffffffffffffffffffffff168173ffffffffffffffffffffffffffffffffffffffff167f8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925856040518082815260200191505060405180910390a3600191505b5092915050565b60003073ffffffffffffffffffffffffffffffffffffffff163190505b90565b60006000339050600160008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020548311158015610480575061047f8585856107f3565b5b156105185782600160008773ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000206000828254039250508190555060019150610522565b60009150610522565b5b509392505050565b6000600060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205482111580156105af57503373ffffffffffffffffffffffffffffffffffffffff166108fc839081150290604051809050600060405180830381858888f193505050505b156106585781600060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600082825403925050819055503373ffffffffffffffffffffffffffffffffffffffff167f7fcf532c15f0a6db0bd6d0e038bea71d30d808c7d98cb3bf7268a95bf5081b65836040518082815260200191505060405180910390a260019050610662565b60009050610662565b5b919050565b6000600060008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205490505b919050565b34600060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600082825401925050819055503373ffffffffffffffffffffffffffffffffffffffff167fe1fffcc4923d04b559f4d29a8bfc6cda04eb5b0d3c460751c2402c5c5cc9109c346040518082815260200191505060405180910390a25b565b600060003390506107618185856107f3565b91505b5092915050565b6000600160008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205490505b92915050565b6000600060008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054821115156109455781600060008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000206000828254039250508190555081600060008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600082825401925050819055508273ffffffffffffffffffffffffffffffffffffffff168473ffffffffffffffffffffffffffffffffffffffff167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef846040518082815260200191505060405180910390a36001905061094f565b6000905061094f565b5b93925050505600a165627a7a72305820813680aa5afcc2e7b1bcc69b5b9b2ea79371f3524fa18da94d56405bd16dc36d0029"
