module EVM.Bytecode
    ( Opcode (..)
    , opcodeToWord8
    , word8ToOpcode
    , EVM.Bytecode.parse
    , EVM.Bytecode.parseLenient
    , isWellFormed
    , assemble
    , opcodeSize
    , programCounters
    , byteStringToHexString
    , hexStringToByteString
    , bytestringToInteger
    , integerToBytes
    ) where

-- TODO(lorenzb): Reorganize internal/external functions

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (char8, endOfLine, isDigit_w8)
import           Data.ByteString                  (ByteString, pack, unpack)
import           Data.Word                        (Word8)
import           Prelude                          hiding (EQ, GT, LT)
import           Text.Printf

data Opcode = STOP
            | ADD
            | MUL
            | SUB
            | DIV
            | SDIV
            | MOD
            | SMOD
            | ADDMOD
            | MULMOD
            | EXP
            | SIGNEXTEND
            | LT
            | GT
            | SLT
            | SGT
            | EQ
            | ISZERO
            | AND
            | OR
            | XOR
            | NOT
            | BYTE
            | SHA3
            | ADDRESS
            | BALANCE
            | ORIGIN
            | CALLER
            | CALLVALUE
            | CALLDATALOAD
            | CALLDATASIZE
            | CALLDATACOPY
            | CODESIZE
            | CODECOPY
            | GASPRICE
            | EXTCODESIZE
            | EXTCODECOPY
            | RETURNDATASIZE
            | RETURNDATACOPY
            | BLOCKHASH
            | COINBASE
            | TIMESTAMP
            | NUMBER
            | DIFFICULTY
            | GASLIMIT
            | POP
            | MLOAD
            | MSTORE
            | MSTORE8
            | SLOAD
            | SSTORE
            | JUMP
            | JUMPI
            | PC
            | MSIZE
            | GAS
            | JUMPDEST
            | PUSH Word8 Integer
            | DUP Word8
            | SWAP Word8
            | LOG0
            | LOG1
            | LOG2
            | LOG3
            | LOG4
            | CREATE
            | CALL
            | CALLCODE
            | RETURN
            | DELEGATECALL
            | STATICCALL
            | REVERT
            | SUICIDE
            | Unknown Word8
            deriving (Show, Eq)

isWellFormed :: [Opcode] -> Bool
isWellFormed = all wf
    where wf (PUSH n x)  = 1 <= n && n <= 32 && 0 <= x && x < 256^n
          wf (DUP n)     = 1 <= n && n <= 16
          wf (SWAP n)    = 1 <= n && n <= 16
          wf (Unknown x) = (Unknown x) == (word8ToOpcode . opcodeToWord8) (Unknown x)
          wf _           = True

word8ToOpcode :: Word8 -> Opcode
word8ToOpcode 0x00 = STOP
word8ToOpcode 0x01 = ADD
word8ToOpcode 0x02 = MUL
word8ToOpcode 0x03 = SUB
word8ToOpcode 0x04 = DIV
word8ToOpcode 0x05 = SDIV
word8ToOpcode 0x06 = MOD
word8ToOpcode 0x07 = SMOD
word8ToOpcode 0x08 = ADDMOD
word8ToOpcode 0x09 = MULMOD
word8ToOpcode 0x0a = EXP
word8ToOpcode 0x0b = SIGNEXTEND
word8ToOpcode 0x10 = LT
word8ToOpcode 0x11 = GT
word8ToOpcode 0x12 = SLT
word8ToOpcode 0x13 = SGT
word8ToOpcode 0x14 = EQ
word8ToOpcode 0x15 = ISZERO
word8ToOpcode 0x16 = AND
word8ToOpcode 0x17 = OR
word8ToOpcode 0x18 = XOR
word8ToOpcode 0x19 = NOT
word8ToOpcode 0x1a = BYTE
word8ToOpcode 0x20 = SHA3
word8ToOpcode 0x30 = ADDRESS
word8ToOpcode 0x31 = BALANCE
word8ToOpcode 0x32 = ORIGIN
word8ToOpcode 0x33 = CALLER
word8ToOpcode 0x34 = CALLVALUE
word8ToOpcode 0x35 = CALLDATALOAD
word8ToOpcode 0x36 = CALLDATASIZE
word8ToOpcode 0x37 = CALLDATACOPY
word8ToOpcode 0x38 = CODESIZE
word8ToOpcode 0x39 = CODECOPY
word8ToOpcode 0x3a = GASPRICE
word8ToOpcode 0x3b = EXTCODESIZE
word8ToOpcode 0x3c = EXTCODECOPY
word8ToOpcode 0x3d = RETURNDATASIZE
word8ToOpcode 0x3e = RETURNDATACOPY
word8ToOpcode 0x40 = BLOCKHASH
word8ToOpcode 0x41 = COINBASE
word8ToOpcode 0x42 = TIMESTAMP
word8ToOpcode 0x43 = NUMBER
word8ToOpcode 0x44 = DIFFICULTY
word8ToOpcode 0x45 = GASLIMIT
word8ToOpcode 0x50 = POP
word8ToOpcode 0x51 = MLOAD
word8ToOpcode 0x52 = MSTORE
word8ToOpcode 0x53 = MSTORE8
word8ToOpcode 0x54 = SLOAD
word8ToOpcode 0x55 = SSTORE
word8ToOpcode 0x56 = JUMP
word8ToOpcode 0x57 = JUMPI
word8ToOpcode 0x58 = PC
word8ToOpcode 0x59 = MSIZE
word8ToOpcode 0x5a = GAS
word8ToOpcode 0x5b = JUMPDEST
word8ToOpcode 0xa0 = LOG0
word8ToOpcode 0xa1 = LOG1
word8ToOpcode 0xa2 = LOG2
word8ToOpcode 0xa3 = LOG3
word8ToOpcode 0xa4 = LOG4
word8ToOpcode 0xf0 = CREATE
word8ToOpcode 0xf1 = CALL
word8ToOpcode 0xf2 = CALLCODE
word8ToOpcode 0xf3 = RETURN
word8ToOpcode 0xf4 = DELEGATECALL
word8ToOpcode 0xfa = STATICCALL
word8ToOpcode 0xfd = REVERT
word8ToOpcode 0xff = SUICIDE
word8ToOpcode w | 0x60 <= w && w < 0x60 + 32 = PUSH (fromIntegral $ w - 0x60 + 1) (2^256)
                | 0x80 <= w && w < 0x80 + 16 = DUP (fromIntegral $ w - 0x80 + 1)
                | 0x90 <= w && w < 0x90 + 16 = SWAP (fromIntegral $ w - 0x90 + 1)
                | otherwise                  = Unknown w

opcodeToWord8 :: Opcode -> Word8
opcodeToWord8 STOP           = 0x00
opcodeToWord8 ADD            = 0x01
opcodeToWord8 MUL            = 0x02
opcodeToWord8 SUB            = 0x03
opcodeToWord8 DIV            = 0x04
opcodeToWord8 SDIV           = 0x05
opcodeToWord8 MOD            = 0x06
opcodeToWord8 SMOD           = 0x07
opcodeToWord8 ADDMOD         = 0x08
opcodeToWord8 MULMOD         = 0x09
opcodeToWord8 EXP            = 0x0a
opcodeToWord8 SIGNEXTEND     = 0x0b
opcodeToWord8 LT             = 0x10
opcodeToWord8 GT             = 0x11
opcodeToWord8 SLT            = 0x12
opcodeToWord8 SGT            = 0x13
opcodeToWord8 EQ             = 0x14
opcodeToWord8 ISZERO         = 0x15
opcodeToWord8 AND            = 0x16
opcodeToWord8 OR             = 0x17
opcodeToWord8 XOR            = 0x18
opcodeToWord8 NOT            = 0x19
opcodeToWord8 BYTE           = 0x1a
opcodeToWord8 SHA3           = 0x20
opcodeToWord8 ADDRESS        = 0x30
opcodeToWord8 BALANCE        = 0x31
opcodeToWord8 ORIGIN         = 0x32
opcodeToWord8 CALLER         = 0x33
opcodeToWord8 CALLVALUE      = 0x34
opcodeToWord8 CALLDATALOAD   = 0x35
opcodeToWord8 CALLDATASIZE   = 0x36
opcodeToWord8 CALLDATACOPY   = 0x37
opcodeToWord8 CODESIZE       = 0x38
opcodeToWord8 CODECOPY       = 0x39
opcodeToWord8 GASPRICE       = 0x3a
opcodeToWord8 EXTCODESIZE    = 0x3b
opcodeToWord8 EXTCODECOPY    = 0x3c
opcodeToWord8 RETURNDATASIZE = 0x3d
opcodeToWord8 RETURNDATACOPY = 0x3e
opcodeToWord8 BLOCKHASH      = 0x40
opcodeToWord8 COINBASE       = 0x41
opcodeToWord8 TIMESTAMP      = 0x42
opcodeToWord8 NUMBER         = 0x43
opcodeToWord8 DIFFICULTY     = 0x44
opcodeToWord8 GASLIMIT       = 0x45
opcodeToWord8 POP            = 0x50
opcodeToWord8 MLOAD          = 0x51
opcodeToWord8 MSTORE         = 0x52
opcodeToWord8 MSTORE8        = 0x53
opcodeToWord8 SLOAD          = 0x54
opcodeToWord8 SSTORE         = 0x55
opcodeToWord8 JUMP           = 0x56
opcodeToWord8 JUMPI          = 0x57
opcodeToWord8 PC             = 0x58
opcodeToWord8 MSIZE          = 0x59
opcodeToWord8 GAS            = 0x5a
opcodeToWord8 JUMPDEST       = 0x5b
opcodeToWord8 (PUSH n _)     = 0x60 - 1 + n
opcodeToWord8 (DUP n )       = 0x80 - 1 + n
opcodeToWord8 (SWAP n )      = 0x90 - 1 + n
opcodeToWord8 LOG0           = 0xa0
opcodeToWord8 LOG1           = 0xa1
opcodeToWord8 LOG2           = 0xa2
opcodeToWord8 LOG3           = 0xa3
opcodeToWord8 LOG4           = 0xa4
opcodeToWord8 CREATE         = 0xf0
opcodeToWord8 CALL           = 0xf1
opcodeToWord8 CALLCODE       = 0xf2
opcodeToWord8 RETURN         = 0xf3
opcodeToWord8 DELEGATECALL   = 0xf4
opcodeToWord8 STATICCALL     = 0xfa
opcodeToWord8 REVERT         = 0xfd
opcodeToWord8 SUICIDE        = 0xff
opcodeToWord8 (Unknown x)    = x

bytestringToInteger :: ByteString -> Integer
bytestringToInteger b = sum $ zipWith (*) (map fromIntegral . reverse . unpack $ b) (iterate (* 256) 1)

integerToBytes :: Word8 -> Integer -> [Word8]
integerToBytes l i = reverse $ Prelude.take (fromIntegral l) (aux i ++ repeat 0)
    where aux 0 = []
          aux i = let (q, r) = i `divMod` 256 in fromIntegral r : aux q


parserOpcode :: Parser Opcode
parserOpcode = do w <- anyWord8
                  case word8ToOpcode w of
                      (PUSH n _) -> try ((PUSH n . bytestringToInteger) <$> Data.Attoparsec.ByteString.take (fromIntegral  n))
                      o          -> return o

parserInvalidEnd :: Parser Opcode
parserInvalidEnd = do takeWhile1 (const True)
                      return (Unknown 0xfe)


parserProgram :: Parser [Opcode]
parserProgram = many parserOpcode <* endOfInput

parserProgramLenient :: Parser [Opcode]
parserProgramLenient = do program <- many parserOpcode
                          suffix <- option [] ((\x -> [x]) <$> parserInvalidEnd)
                          endOfInput
                          return (program ++ suffix)


pairs :: [a] -> [(a,a)]
pairs []        = []
pairs [_]       = error "Single input"
pairs (x:x':xs) = (x, x') : pairs xs

hexDigit :: Char -> Word8
hexDigit x | x == '0' = 0x0
           | x == '1' = 0x1
           | x == '2' = 0x2
           | x == '3' = 0x3
           | x == '4' = 0x4
           | x == '5' = 0x5
           | x == '6' = 0x6
           | x == '7' = 0x7
           | x == '8' = 0x8
           | x == '9' = 0x9
           | x == 'a' = 0xa
           | x == 'b' = 0xb
           | x == 'c' = 0xc
           | x == 'd' = 0xd
           | x == 'e' = 0xe
           | x == 'f' = 0xf
           | otherwise = error "invalid hex digit"

hexStringToByteString :: String -> ByteString
hexStringToByteString = pack . map (\(n1,n2) -> n1 * 16 + n2) . pairs . map fromIntegral . map hexDigit

byteStringToHexString :: ByteString -> String
byteStringToHexString = concatMap (printf "%02x") . unpack

parse :: ByteString -> Either String [Opcode]
parse = parseOnly parserProgram

parseLenient :: ByteString -> Either String [Opcode]
parseLenient = parseOnly parserProgramLenient

assemble :: [Opcode] -> ByteString
assemble ops = if isWellFormed ops
               then pack . concatMap aux $ ops
               else error "assemble: ops isn't well-formed"
    where aux (PUSH n x) = opcodeToWord8 (PUSH n x) : integerToBytes n x
          aux op         = [opcodeToWord8 op]

opcodeSize :: Opcode -> Integer
opcodeSize (PUSH n _) = 1 + fromIntegral n
opcodeSize _          = 1

programCounters :: [Opcode] -> [Integer]
programCounters = scanl (+) 0 . map opcodeSize
