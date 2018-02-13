module Main where

import           Data.Semigroup ((<>))
import qualified EVM.Address as A
import           EVM.Bytecode
import           EVM.GenericInitcode
import qualified EVM.Instrumentation as I
import           EVM.Instrumentation.Metacontract
import           Options.Applicative
import           System.Exit
import           System.IO


data ParsingMode = Strict
                 | Lenient
                 deriving (Eq, Show)

parseParsingMode :: Parser ParsingMode
parseParsingMode = flag Strict Lenient
  ( long "lenient"
 <> help ( "Lenient byte code parsing: truncated PUSH instructions "
        ++ "will be filled with zeros."
         )
  )

data InstrumentMode = First | Nth deriving (Eq, Show)

parseInstrumentMode :: Parser InstrumentMode
parseInstrumentMode =
      flag' First ( long "first"
                 <> help "Instrument first head")
  <|> flag' Nth ( long "nth"
               <> help "Instrument n-th head")

data Command = Disassemble String
             | Instrument InstrumentMode A.Address String
             | Metacontract [A.Address]
             deriving (Eq, Show)

parseAddress :: ReadM A.Address
parseAddress = eitherReader A.parse

parseCommand :: Parser Command
parseCommand = subparser
  ( command "disassemble"
    ( info
      ( helper
    <*> ( Disassemble
      <$> strArgument ( metavar "CONTRACT"
                     <> help ( "Hex-encoded EVM bytecode of contract to be "
                            ++ "disassembled."
                             )
                      )
        )
      )
      ( progDesc "Disassemble EVM byte code."
      )
    )
 <> command "instrument"
    ( info
      ( helper
    <*> ( Instrument
      <$> parseInstrumentMode
      <*> option parseAddress
                 ( long "metacontract"
                <> metavar "ADDRESS"
                <> help ( "Address of metacontract. The address is "
                       ++ "hardcoded into the instrumented contract."
                        )
                 )
      <*> strArgument (metavar "CONTRACT")
        )
      )
      ( progDesc "Instrument a contract to turn it into a head."
      )
    )
 <> command "metacontract"
    ( info
      ( helper
    <*> ( Metacontract
      <$> some ( argument parseAddress
                 ( metavar "ADDRESS..."
                <> help ( "Addresses of the heads. These addresses are "
                       ++ "hardcoded into the metacontract. The code at "
                       ++ "the 1st address must have been instrumented "
                       ++ "with `instrument --first`, the code at the 2nd, "
                       ++ "3rd, ... addresses must have been instrumented "
                       ++ "with `instrument --nth`.")
                 )
               )
        )
      )
      ( progDesc "Generate metacontract."
      )
    )
  )

data Options = Options ParsingMode Command deriving (Eq, Show)

parseOptions :: Parser Options
parseOptions = Options <$> parseParsingMode <*> parseCommand

infoOptions :: ParserInfo Options
infoOptions = info (helper <*> parseOptions) (progDesc "Hydra instrumenter")

run :: Options -> Either String String
run (Options pmod cmd) = do
  let parseContract = case pmod of
                        Strict -> parse . hexStringToByteString
                        Lenient -> parseLenient . hexStringToByteString
  case cmd of
    (Disassemble s) -> parseContract s >>= return . unlines . map show
    (Instrument imod a s) -> do
      let instrument = case imod of
                         First -> I.instrumentFirst
                         Nth -> I.instrumentNth
      parsed <- parseContract s
      instrumented <- instrument a parsed
      return . byteStringToHexString . assemble $ genericInitcode ++ instrumented
    (Metacontract as) -> return . byteStringToHexString . assemble $ genericInitcode ++ (metacontract as)


eitherIO :: Either String String -> IO ()
eitherIO (Left s) = hPutStrLn stderr s >> exitWith (ExitFailure 1)
eitherIO (Right s) = putStrLn s

main :: IO ()
main = execParser infoOptions >>= eitherIO . run
