module EVM.Address
( Address()
, fromInteger
, toInteger
, print
, parse
) where

import           Control.Monad
import           Numeric
import           Prelude hiding (fromInteger, toInteger, print)
import           Text.Printf

newtype Address = Address Integer deriving (Show, Eq)

fromInteger :: Integer -> Address
fromInteger i | 0 <= i && i < 2^160 = Address i
              | otherwise            = error "Invalid Address"

toInteger :: Address -> Integer
toInteger (Address i) = i

print :: Address -> String
print (Address i) = printf "0x%040x" i

parse :: String -> Either String Address
parse s = do
  when (length s /= 42) (Left "Address must be 42 characters long")
  when (take 2 s /= "0x") (Left "Address must start with 0x")
  let s' = drop 2 s
  when (any (\x -> x `notElem` "0123456789abcdef") s')
       (Left "Address must be lower-case hex")
  return . Address . fst . head $ readHex s'
