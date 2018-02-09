{-# LANGUAGE OverloadedStrings #-}

module EVM.Instrumentation
    ( instrumentFirst
    , instrumentNth
    ) where

import           Data.Function
import           Data.List
import           Data.Maybe
import           EVM.Bytecode     (Opcode (..))
import           EVM.BytecodePlus (OpcodePlus (..), lift, lower)
import qualified EVM.Instrumentation.HeadOne as HO
import qualified EVM.Instrumentation.HeadN as HN
import           EVM.While
import           Prelude          hiding (EQ, GT, LT)
import           Text.Printf
import           Util

instrumentFirst :: Integer -> [Opcode] -> Either String [Opcode]
instrumentFirst mc contract = instrument HO.instrumentOps HO.procs mc contract

instrumentNth :: Integer -> [Opcode] -> Either String [Opcode]
instrumentNth mc contract = instrument HN.instrumentOps HN.procs mc contract

instrument :: (Integer -> [OpcodePlus] -> [OpcodePlus]) -> (Integer -> [Proc]) -> Integer -> [Opcode] -> Either String [Opcode]
instrument instrumentOps procs mc contract =
    do let ps = procs mc
       let cps = concatMap compProc ps
       showError . checkInstrumentable $ contract
       let (contract', pcs2tags) = lift (contract ++ [STOP])
       let jt = jumptable pcs2tags
       let contract'' = instrumentOps mc contract'
       let contract''' = [ProcedureCall $ procTag "init"
                         , Op POP]
                         ++ contract''
                         ++ cps
                         ++ jt
       lower contract'''

checkInstrumentable :: [Opcode] -> Either [(Int, String)] ()
checkInstrumentable ops = case errors of {[] -> Right (); _ -> Left errors}
    where errors = catMaybes $ findForbidden ops ++ checkPC ops
          findForbidden :: [Opcode] -> [Maybe (Int, String)]
          findForbidden = zipWith (\n op ->
                                       if op `elem` forbidden
                                       then Just (n, show op ++ " is a forbidden operation. Cannot instrument.")
                                       else Nothing) [1..]
          forbidden = [ CODESIZE, CODECOPY
                      , EXTCODESIZE
                      , EXTCODECOPY
                      , CREATE, CALLCODE
                      , DELEGATECALL, SUICIDE
                      -- Metropolis opcodes
                      , STATICCALL, RETURNDATACOPY, RETURNDATASIZE
                      ]
          checkPC :: [Opcode] -> [Maybe (Int, String)]
          checkPC []   = [Nothing]
          checkPC [PC] = [Just (1, "PC not follwed by JUMP/JUMPI is a forbidden operation. Cannot instrument.")]
          checkPC (op:ops) = zipWith3 aux [1..] (op:ops) ops
          aux n PC JUMP = Nothing
          aux n PC JUMPI = Nothing
          aux n PC _ = Just (n, "PC not follwed by JUMP/JUMPI is a forbidden operation. Cannot instrument.")
          aux n _ _ = Nothing

data JumpTree = Empty | Node Integer String JumpTree JumpTree deriving (Show, Eq)

buildJumpTree :: [(Integer, String)] -> JumpTree
buildJumpTree = aux . sortBy (compare `on` fst)
    where aux [] = Empty
          aux xs = let middle = length xs `div` 2
                       left = take middle xs
                       median = xs !! middle
                       right = drop (middle + 1) xs
                       (i, t) = median
                   in Node i t (aux left) (aux right)

jumpTreeCode :: JumpTree -> [OpcodePlus]
jumpTreeCode jt = begin ++ aux jt
    where tag Empty = procTag "unknownJumpdest" -- This depends on details of how ProcedureCalls work
          tag (Node i _ _ _ ) = printf "jumptree_%d" i
          aux Empty = []
          aux n@(Node i t j1 j2) = concat [ aux j1
                                          , aux j2
                                          , [ TagJumpdest (tag n) -- [old destination]
                                            , Op $ DUP 1          -- [old destination, old destination]
                                            , Push i              -- [potential old destination, old destination, old destination]
                                            , Op $ GT             -- [old destination < potential old destination, old destination]
                                            , TagJumpi (tag j1)   -- [old destination]
                                            , Op $ DUP 1          -- [old destination, old destination]
                                            , Push i              -- [potential old destination, old destination, old destination]
                                            , Op $ LT             -- [potential old destination < old destination, old destination]
                                            , TagJumpi (tag j2)   -- [old destination]
                                            , Op $ POP            -- []
                                            , TagJump t
                                            ]
                                          ]
          begin = [ TagJumpdest "jumptable" -- [old destination]
                  , TagJump (tag jt)
                  ]

jumptable :: [(Integer, String)] -> [OpcodePlus]
jumptable = jumpTreeCode . buildJumpTree
