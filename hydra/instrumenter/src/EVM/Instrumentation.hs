{-# LANGUAGE OverloadedStrings #-}

module EVM.Instrumentation
    ( instrumentFirst
    , instrumentNth
    ) where

import           Control.Monad
import qualified Data.ByteString  as B (length)
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Word
import           EVM.Bytecode     (Opcode (..), assemble, opcodeSize)
import           EVM.BytecodePlus (OpcodePlus (..), lift, lower)
import qualified EVM.Instrumentation.HeadOne as HO
import qualified EVM.Instrumentation.HeadN as HN
import           EVM.While
import qualified EVM.While.Macros as M
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
       let contract''' = [ProcedureCall $ procTag "init"]
                         ++ contract''
                         ++ cps
                         ++ jt
       lower contract'''
    -- do let ps = procs mc
    --    sequence_ $ map (\p@(Procedure name _ _ _) -> (showError . consError name . checkProcedure ps) p) ps
    --    showError . checkInstrumentable $ contract
    --    let (contract', pcs2tags) = lift contract
    --    let jt = jumptable pcs2tags
    --    let contract'' = instrumentOps mc contract'
    --    let checkableContract = InstrumentedContract ps jt contract''
    --    let contract''' = link checkableContract
    --    lower contract'''


checkInstrumentable :: [Opcode] -> Either (Int, String) ()
checkInstrumentable ops = checkForbidden ops >> checkPC ops
    where checkForbidden = sequence_ . zipWith (\n op ->
                               if op `elem` forbidden
                               then Left (n, show op ++ " is a forbidden operation. Cannot instrument.")
                               else Right ()) [1..]
          forbidden = [ CODESIZE, CODECOPY
                      , EXTCODESIZE, EXTCODECOPY
                      , CREATE, CALLCODE
                      , DELEGATECALL, SUICIDE
                      -- Metropolis opcodes
                      , STATICCALL, RETURNDATACOPY, RETURNDATASIZE
                      -- TODO(lorenzb): Deal with REVERT properly
                      ]
          checkPC [] = Right ()
          checkPC [PC] = Left (1, "PC not follwed by JUMP/JUMPI is a forbidden operation. Cannot instrument.")
          checkPC (op:ops) = sequence_ (zipWith3 aux [1..] (op:ops) ops)
          aux n PC JUMP = Right ()
          aux n PC JUMPI = Right ()
          aux n PC _ = Left (n, "PC not follwed by JUMP/JUMPI is a forbidden operation. Cannot instrument.")
          aux n _ _ = Right()

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
                                          , -- fromRight . consError "jumptree" . checkOps [] $
                                          [ TagJumpdest (tag n) -- ⤳ S [V "old destination"]
                                          , Op $ DUP 1               -- ⤳ S [V "old destination", V "old destination"]
                                          , Push i              -- ⤳ S [N "potential old destination", V "old destination", V "old destination"]
                                          , Op $ GT                  -- ⤳ S [N "old destination < potential old destination", V "old destination"]
                                          , TagJumpi (tag j1)   -- ⤳ S [V "old destination"]
                                          , Op $ DUP 1               -- ⤳ S [V "old destination", V "old destination"]
                                          , Push i              -- ⤳ S [N "potential old destination", V "old destination", V "old destination"]
                                          , Op $ LT                  -- ⤳ S [N "potential old destination < old destination", V "old destination"]
                                          , TagJumpi (tag j2)   -- ⤳ S [V "old destination"]
                                          , Op $ POP                 -- ⤳ S []
                                          , TagJump t           -- ⤳ Ø
                                          ]
                                          ]
          begin = -- fromRight . consError "jumptable begin" . checkOps [] $
                  [ TagJumpdest "jumptable"      -- ⤳ S [V "old destination"]
                  , TagJump (tag jt)             -- ⤳ Ø
                  ]

jumptable :: [(Integer, String)] -> [OpcodePlus]
jumptable = jumpTreeCode . buildJumpTree
