module EVM.BytecodePlus
( OpcodePlus (..)
, lift
, lower
) where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Word
import EVM.Bytecode (Opcode(..), opcodeSize, programCounters)
import Text.Printf

data OpcodePlus = Op Opcode
         | TagJumpdest String
         | TagJump String
         | TagJumpi String
         | TagPush String
         | Push Integer
         | ProcedureCall String
         | Nop
         deriving (Show, Eq)

failNothing :: a -> Maybe b -> Either a b
failNothing x (Just y) = Right y
failNothing x Nothing  = Left x

lift :: [Opcode] -> ([OpcodePlus], [(Integer, String)])
lift contract = (contractWithTaggedJumpdests,  jumpdestpcsToTags)
    where contractWithPCs = zip contract (programCounters contract)
          pc2tag pc = printf "oldpc%d" pc
          aux (JUMPDEST, pc) = Just (pc, pc2tag pc)
          aux _              = Nothing
          jumpdestpcsToTags = mapMaybe aux contractWithPCs
          aux2 (JUMPDEST, pc) = TagJumpdest (pc2tag pc)
          aux2 (op, _)        = Op op
          contractWithTaggedJumpdests = map aux2 contractWithPCs
          aux3 [] = []
          aux3 [x] = [x]
          aux3 (Op (PUSH w pc) : Op JUMP : xs) = case lookup pc jumpdestpcsToTags of
                                                     (Just t) -> TagJump t : aux3 xs
                                                     Nothing  -> Op (PUSH w pc) : Op JUMP : aux3 xs
          aux3 (Op (PUSH w pc) : Op JUMPI : xs) = case lookup pc jumpdestpcsToTags of
                                                      (Just t) -> TagJumpi t : aux3 xs
                                                      Nothing  -> Op (PUSH w pc) : Op JUMPI : aux3 xs
          aux3 (x : x' : xs) = x : aux3 (x' : xs)
          contractWithStaticJumps = aux3 contractWithTaggedJumpdests


lowerStage1 :: [OpcodePlus] -> Either a [OpcodePlus]
lowerStage1 = return . concatMap aux . zip [1..]
    where aux (_, TagJump t)       = [ TagPush t
                                     , Op JUMP
                                     ]
          aux (_, TagJumpi t)      = [ TagPush t
                                     , Op JUMPI
                                     ]
          aux (_, Push i)          = [ Op (PUSH (width i) i) ]
          aux (i, ProcedureCall t) = let continue = "continue" ++ show i in
                                     [ TagPush continue
                                     , TagPush t
                                     , Op JUMP
                                     , TagJumpdest continue
                                     ]
          aux (_, Nop)             = []
          aux (_, op)              = [ op ]

width :: Integer -> Word8
width x = head [i  | i <- [1..32], x < 256^i]

tags2pcs :: Word8 -> [OpcodePlus] -> [(String, Integer)]
tags2pcs tagPushWidth contract = checkKeys $ mapMaybe aux $ zip (pcs contract) contract
    where opcodePlusSize (Op op)         = opcodeSize op
          opcodePlusSize (TagJumpdest t) = opcodeSize JUMPDEST
          opcodePlusSize (TagPush _)     = opcodeSize (PUSH tagPushWidth 0)
          opcodePlusSize _               = error "unsupported op in tags2pcs"
          pcs = scanl (+) 0 . map opcodePlusSize
          aux (i, TagJumpdest t) = Just (t, i)
          aux _                  = Nothing

checkKeys :: (Show a, Eq a) => [(a, b)] -> [(a, b)]
checkKeys xs = let fxs = map fst xs in if fxs == nub fxs then xs else error $ "duplicate key in: " ++ show fxs

lowerStage2 :: [OpcodePlus] -> Either String [Opcode]
lowerStage2 ops = sequence . map aux $ ops
    where tagwidth = fromIntegral . width . maximum . map snd . tags2pcs 32 $ ops
          aux (Op op)         = return op
          aux (TagJumpdest _) = return JUMPDEST
          aux (TagPush t)     = do pc <- failNothing (t ++ " not found") (lookup t $ tags2pcs tagwidth ops)
                                   return $ PUSH tagwidth pc

lower :: [OpcodePlus] -> Either String [Opcode]
lower = lowerStage1 >=> lowerStage2
