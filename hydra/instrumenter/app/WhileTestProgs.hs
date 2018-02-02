module Main where

import           Control.Monad
import           Data.Char
import qualified Data.Text           as T
import           EVM.Bytecode
import           EVM.GenericInitcode
import           EVM.Instrumentation.Internal
import           EVM.While
import           System.Environment
import           System.Exit
import           System.IO


add = Proc "add" ["a", "b"] "c" (Scope
      [(Assign "c" (Add (Var "b") (Var "a")))])

multiply = Proc "multiply" ["a", "b"] "c" (Scope
           [(While (Lt (Lit 0) (Var "a"))
                (Scope [(Assign "c" (ProcCall "add" [(Var "c")
                                                    ,(Var "b")]))
                       ,(Assign "a" (Sub (Var "a") (Lit 1)))]))])

square = Proc "square" ["a"] "c" (Scope [(Assign "c" (ProcCall "multiply" [(Var "a"), (Var "a")]))])

squareProg = Scope [(Mstore (Lit 0x0) (ProcCall "square" [(Calldataload (Lit 0x0))]))
                   ,(Return (Lit 0x0) (Lit 0x20))]

squareProgCompiled = compileAndLower [add, multiply, square] squareProg

ownAddress = Proc "ownAddress" [] "address" (Scope [(Let "address2" Address)
                                                   ,(Assign "address" (Var "address2"))])

selfCallProg = Scope [IfElse (Iszero (Eq (ProcCall "ownAddress" []) Caller))
                          (Scope [(Discard (Call Gas Address (Lit 0) (Lit 0x0) (Lit 0x0) (Lit 0x0) (Lit 0x40)))
                                 ,(Return (Lit 0x20) (Lit 0x20))])
                          (Scope [(Let "output" (Lit 0x1337133713371337133713371337133713371337133713371337133713371334))
                                 ,(Let "foo" (Lit 0))
                                 ,(IfElse (Lit 1)
                                      (Scope [(Assign "output" (Add (Var "output") (Lit 1)))
                                             ,(Let "extra" (Lit 1))
                                             ,(Assign "extra" (Mul (Var "extra") (Lit 2)))
                                             ,(Assign "foo" (Var "extra"))])
                                      (Scope []))
                                 ,(Assign "output" (Add (Var "output") (Var "foo")))
                                 ,(Mstore (Lit 0x20) (Var "output"))
                                 ,(Return (Lit 0x0) (Lit 0x40))])]

selfCallProgCompiled = compileAndLower [ownAddress] selfCallProg

memcpyProg name = Scope [(Mstore (Lit 0x0) (Lit 0xe7))
                   -- 00000000000000000000000000000000000000000000000000000000000000e7
                   ,(Discard (ProcCall name [(Lit 0x1e), (Lit 0x1f), (Lit 1)]))
                   -- 000000000000000000000000000000000000000000000000000000000000e7e7
                   ,(Discard (ProcCall name [(Lit 0x1c), (Lit 0x1e), (Lit 2)]))
                   -- 00000000000000000000000000000000000000000000000000000000e7e7e7e7
                   ,(Discard (ProcCall name [(Lit 0x18), (Lit 0x1c), (Lit 4)]))
                   -- 000000000000000000000000000000000000000000000000e7e7e7e7e7e7e7e7
                   ,(Discard (ProcCall name [(Lit 0x10), (Lit 0x18), (Lit 8)]))
                   -- 00000000000000000000000000000000e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7
                   ,(Discard (ProcCall name [(Lit 0x10), (Lit 0x18), (Lit 8)]))
                   -- 00000000000000000000000000000000e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7
                   ,(Discard (ProcCall name [(Lit 0x20), (Lit 0x00), (Lit 0x20)]))
                   -- 00000000000000000000000000000000e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e700000000000000000000000000000000e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7
                   ,(Discard (ProcCall name [(Lit 0x00), (Lit 0x10), (Lit 0x20)]))
                   -- e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e70000000000000000000000000000000000000000000000000000000000000000e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7
                   ,(Discard (ProcCall name [(Lit 0x00), (Lit 0x10), (Lit 0x00)]))
                   -- e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e70000000000000000000000000000000000000000000000000000000000000000e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7
                   ,(Mstore8 (Lit 0x1f) (Byte (Lit 31) Msize))
                   -- e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e70000000000000000000000000000004000000000000000000000000000000000e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7
                   ,(Discard (ProcCall name [(Lit 0x21), (Lit 0x00), (Lit 0x21)]))
                   -- e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e70000000000000000000000000000004000e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e70000000000000000000000000000004000
                   ,(Return (Lit 0x00) (Lit 0x60))]

memcpyProgCompiled = compileAndLower [procMemcpy] (memcpyProg "memcpy")

memcpy2ProgCompiled = compileAndLower [procMemcpy2] (memcpyProg "memcpy2")

minProg = Scope [(Mstore (Lit 0x1337) (ProcCall "min" [(Calldataload (Lit 0x00)), ((Calldataload (Lit 0x20)))]))
                ,(Return (Lit 0x1337) (Lit 0x20))]

minProgCompiled = compileAndLower [procMin] minProg

printErrorAndExit :: (Show a) => Either a b -> IO b
printErrorAndExit (Left x)  = do hPutStrLn stderr $ show x
                                 exitWith (ExitFailure 1)
printErrorAndExit (Right x) = return x


run :: [String] -> IO ()
run args = do p <- printErrorAndExit (aux args)
              putStrLn . byteStringToHexString . assemble $ genericInitcode ++ p
    where aux ["square"]   = squareProgCompiled
          aux ["selfCall"] = selfCallProgCompiled
          aux ["memcpy"]   = memcpyProgCompiled
          aux ["memcpy2"]  = memcpy2ProgCompiled
          aux ["min"]      = minProgCompiled
          aux _            = Left $ unlines [ "Invalid option." ]

main :: IO ()
main = getArgs >>= run
