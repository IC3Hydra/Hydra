{-# LANGUAGE OverloadedStrings #-}

module EVM.Instrumentation
    ( instrument
    , genericInitcode
    ) where

import           Control.Monad
import qualified Data.ByteString as B (length)
import           Data.List
import           Data.Maybe
import           Data.Word
import           EVM.Bytecode    (Opcode (..), assemble, opcodeSize,
                                  programCounters)
import           Prelude         hiding (EQ, GT, LT)

data OpcodePlus = Op Opcode
                | TagJumpdest String
                | TagJump String
                | TagJumpi String
                | TagPush String
                | Push Integer
                | ProcedureCall String
                | ProcedureReturn
                | StateForce
                | Nop
                deriving (Show, Eq)

data StackItem = N String | V String | Ret | Rename String String deriving (Eq, Show)

data State = S [StackItem] | Ø deriving (Eq, Show)

data OpcodePlusWithState = OpcodePlusWithState OpcodePlus State deriving (Eq, Show)

data Procedure = Procedure String [StackItem] State [OpcodePlusWithState]

data InstrumentedContract = InstrumentedContract [Procedure] [OpcodePlus] [OpcodePlus]

-- stack annotation operator "⤳"
-- assign lowest possible precedence
infix 0 ⤳

class OpcodePlusWithStateFrom a where
    (⤳) :: a -> State -> OpcodePlusWithState

instance OpcodePlusWithStateFrom OpcodePlus where
    (⤳) op state = OpcodePlusWithState op state

instance OpcodePlusWithStateFrom Opcode where
    (⤳) op state = OpcodePlusWithState (Op op) state

showError :: (Show a) => Either a b -> Either String b
showError (Left l)  = Left . show $ l
showError (Right r) = Right r

consError :: c -> Either a b -> Either (c, a) b
consError e' = either (\e -> Left (e',e)) Right

fromRight :: (Show a) => Either a b -> b
fromRight = either (\x -> error $ "unexpected Left" ++ show x) id

failNothing :: a -> Maybe b -> Either a b
failNothing x (Just y) = Right y
failNothing x Nothing  = Left x

instrument :: Integer -> [Opcode] -> Either String [Opcode]
instrument mc contract =
    do let ps = procs mc
       sequence_ $ map (\p@(Procedure name _ _ _) -> (showError . consError name . checkProcedure ps) p) ps
       showError . checkInstrumentable $ contract
       let (contract', pcs2tags) = lift contract
       let jt = jumptable pcs2tags
       let contract'' = instrumentOps mc contract'
       let checkableContract = InstrumentedContract ps jt contract''
       let contract''' = link checkableContract
       lower contract'''

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
                      ]
          checkPC [] = Right ()
          checkPC [PC] = Left (1, "PC not follwed by JUMP/JUMPI is a forbidden operation. Cannot instrument.")
          checkPC (op:ops) = sequence_ (zipWith3 aux [1..] (op:ops) ops)
          aux n PC JUMP = Right ()
          aux n PC JUMPI = Right ()
          aux n PC _ = Left (n, "PC not follwed by JUMP/JUMPI is a forbidden operation. Cannot instrument.")
          aux n _ _ = Right()

oldProgramCountersToTags :: [Opcode] -> [(Integer, String)]
oldProgramCountersToTags contract = mapMaybe aux (zip contract (programCounters contract))
    where aux (JUMPDEST, pc) = Just (pc, "oldpc" ++ show pc)
          aux _              = Nothing

lift :: [Opcode] -> ([OpcodePlus], [(Integer, String)])
lift contract = (map aux2 contractWithPCs,  mapMaybe aux contractWithPCs)
    where pc2tag pc = "oldpc" ++ show pc
          aux (JUMPDEST, pc) = Just (pc, pc2tag pc)
          aux _              = Nothing
          aux2 (JUMPDEST, pc) = TagJumpdest (pc2tag pc)
          aux2 (op, _)        = Op op
          contractWithPCs = zip contract (programCounters contract)

jumptable :: [(Integer, String)] -> [OpcodePlus]
jumptable = (begin ++) . (++ end) . concatMap aux
    where aux (i, t) = fromRight . consError "jumptable" . checkOps [] $
                       [ StateForce          ⤳ S [V "old destination"]
                       , TagPush t           ⤳ S [N "potential new destination", V "old destination"]
                       , DUP 2               ⤳ S [V "old destination", V "potential new destination", V "old destination"]
                       , Push i              ⤳ S [N "potential old destination", V "old destination", V "potential new destination", V "old destination"]
                       , EQ                  ⤳ S [N "potential old destination == old destination", V "potential new destination", V "old destination"]
                       , TagJumpi "makejump" ⤳ S [V "potential new destination", V "old destination"]
                       , POP                 ⤳ S [V "old destination"]
                       ]
          begin = fromRight . consError "jumptable begin" . checkOps [] $
                  [ StateForce              ⤳ Ø
                  , TagJumpdest "makejump"  ⤳ S [V "new destination", V "old destination"]
                  , SWAP 1                  ⤳ S [V "old destination", V "new destination"]
                  , POP                     ⤳ S [V "new destination"]
                  , Op JUMP                 ⤳ Ø
                  , TagJumpdest "jumptable" ⤳ S [V "old destination"]
                  ]
          end = [ ProcedureCall "die" ]

instrumentOps :: Integer -> [OpcodePlus] -> [OpcodePlus]
instrumentOps mc = concatMap aux
    where aux (Op ADDRESS)      = fromRight . consError "instr ADDRESS" . checkOps sigs $
                                  [ StateForce ⤳ S []
                                  , Push mc    ⤳ S [N "mc_address"]
                                  ]
          aux (Op BALANCE)      = fromRight . consError "instr BALANCE" . checkOps sigs $
                                  [ StateForce              ⤳ S [N "address"]
                                  , ProcedureCall "balance" ⤳ S [N "balance"]
                                  ]
          aux (Op CALLER)       = fromRight . consError "instr CALLER" . checkOps sigs $
                                  [ StateForce       ⤳ S []
                                  , Push 0           ⤳ S [N "0"]
                                  , CALLDATALOAD     ⤳ S [N "calldata[0]"]
                                  ]
          aux (Op CALLVALUE)    = fromRight . consError "instr CALLVALUE" . checkOps sigs $
                                  [ StateForce      ⤳ S []
                                  , Push 0x20       ⤳ S [N "0x20"]
                                  , CALLDATALOAD    ⤳ S [N "calldata[0x20]"]
                                  ]
          aux (Op CALLDATALOAD) = fromRight . consError "instr CALLDATALOAD" . checkOps sigs $
                                  [ StateForce             ⤳ S [N "offset"]
                                  , Push calldataStashSize ⤳ S [N "calldata stash size", V "offset"]
                                  , ADD                    ⤳ S [N "offset + calldata stash size"]
                                  , CALLDATALOAD           ⤳ S [N "C[offset + calldata stash size]"]
                                  ]
          aux (Op CALLDATASIZE) = fromRight . consError "instr CALLDATASIZE" . checkOps sigs $
                                  [ StateForce             ⤳ S []
                                  , CALLDATASIZE           ⤳ S [N "calldata size"]
                                  , Push calldataStashSize ⤳ S [N "calldata stash size", V "calldata size"]
                                  , (SWAP 1)               ⤳ S [V "calldata size", V "calldata stash size"]
                                  , SUB                    ⤳ S [N "calldata size - calldata stash size"]
                                  ]
          aux (Op CALLDATACOPY) = fromRight . consError "instr CALLDATACOPY" . checkOps sigs $
                                  [ StateForce             ⤳ S [N "dst offset", N "src offset", N "size"]
                                  , Push memoryStashSize   ⤳ S [N "memory stash size", V "dst offset", V "src offset", V "size"]
                                  , ADD                    ⤳ S [N "dst offset + memory stash size", V "src offset", V "size"]
                                  , (SWAP 1)               ⤳ S [V "src offset", V "dst offset + memory stash size", V "size"]
                                  , Push calldataStashSize ⤳ S [N "calldata stash size", V "src offset", V "dst offset + memory stash size", V "size"]
                                  , ADD                    ⤳ S [N "src offset + calldata stash size", V "dst offset + memory stash size", V "size"]
                                  , (SWAP 1)               ⤳ S [V "dst offset + memory stash size", V "src offset + calldata stash size", V "size"]
                                  , CALLDATACOPY           ⤳ S []
                                  ]
          aux (Op MLOAD)        = fromRight . consError "instr MLOAD" . checkOps sigs $
                                  [ StateForce           ⤳ S [N "offset"]
                                  , Push memoryStashSize ⤳ S [N "memory stash size", V "offset"]
                                  , ADD                  ⤳ S [N "offset + memory stash size"]
                                  , MLOAD                ⤳ S [N "M[offset + memory stash size]"]
                                  ]
          aux (Op MSTORE)       = fromRight . consError "instr MSTORE" . checkOps sigs $
                                  [ StateForce           ⤳ S [N "offset", N "word"]
                                  , Push memoryStashSize ⤳ S [N "memory stash size", V "offset", V "word"]
                                  , ADD                  ⤳ S [N "offset + memory stash size", V "word"]
                                  , MSTORE               ⤳ S []
                                  ]
          aux (Op MSTORE8)      = fromRight . consError "instr MSTORE8" . checkOps sigs $
                                  [ StateForce           ⤳ S [N "offset", N "byte"]
                                  , Push memoryStashSize ⤳ S [N "memory stash size", V "offset", V "byte"]
                                  , ADD                  ⤳ S [N "offset + memory stash size", V "byte"]
                                  , MSTORE8              ⤳ S []
                                  ]
          aux (Op SHA3)         = fromRight . consError "instr SHA3" . checkOps sigs $
                                  [ StateForce           ⤳ S [N "offset", N "size"]
                                  , Push memoryStashSize ⤳ S [N "memory stash size", V "offset", V "size"]
                                  , ADD                  ⤳ S [N "offset + memory stash size", V "size"]
                                  , SHA3                 ⤳ S [N "H(M[offset + memory stash size : offset + memory stash size + size])"]
                                  ]
          aux (Op JUMP)         = fromRight . consError "instr JUMP" . checkOps sigs $
                                  [ StateForce           ⤳ S [N "pc"]
                                  , TagJump "jumptable"  ⤳ Ø
                                  ]
          aux (Op JUMPI)        = fromRight . consError "instr JUMPI" . checkOps sigs $
                                  [ StateForce           ⤳ S [N "pc", N "should_jump"]
                                  , (SWAP 1)             ⤳ S [V "should_jump", V "pc"]
                                  , TagJumpi "jumptable" ⤳ S [V "pc"]
                                  , POP                  ⤳ S []
                                  ]
          aux (Op MSIZE)        = fromRight . consError "instr MSIZE" . checkOps sigs $
                                  [ StateForce           ⤳ S []
                                  , MSIZE                ⤳ S [N "msize"]
                                  , Push memoryStashSize ⤳ S [N "memory stash size", V "msize"]
                                  , (SWAP 1)             ⤳ S [V "msize", V "memory stash size"]
                                  , SUB                  ⤳ S [N "msize - memory stash size"]
                                  ]
          aux (Op LOG0)         = fromRight . consError "instr LOG0" . checkOps sigs $
                                  [ StateForce          ⤳ S [N "in offset", N "in size", N "placeholder", N "placeholder", N "placeholder", N "placeholder"]
                                  , Push 0              ⤳ S [N "0", V "in offset", V "in size", V "placeholder", V "placeholder", V "placeholder", V "placeholder"]
                                  , Nop                 ⤳ S ["0" `Rename` "num_topics", V "in offset", V "in size", "placeholder" `Rename` "topic1", "placeholder" `Rename` "topic2", "placeholder" `Rename` "topic3", "placeholder" `Rename` "topic4"]
                                  , ProcedureCall "log" ⤳ S []
                                  ]
          aux (Op LOG1)         = fromRight . consError "instr LOG1" . checkOps sigs $
                                  [ StateForce          ⤳ S [N "in offset", N "in size", N "topic1", N "placeholder", N "placeholder", N "placeholder"]
                                  , Push 1              ⤳ S [N "1", V "in offset", V "in size", V "topic1", V "placeholder", V "placeholder", V "placeholder"]
                                  , Nop                 ⤳ S ["1" `Rename` "num_topics", V "in offset", V "in size", V "topic1", "placeholder" `Rename` "topic2", "placeholder" `Rename` "topic3", "placeholder" `Rename` "topic4"]
                                  , ProcedureCall "log" ⤳ S []
                                  ]
          aux (Op LOG2)         = fromRight . consError "instr LOG2" . checkOps sigs $
                                  [ StateForce          ⤳ S [N "in offset", N "in size", N "topic1", N "topic2", N "placeholder", N "placeholder"]
                                  , Push 2              ⤳ S [N "2", V "in offset", V "in size", V "topic1", V "topic2", V "placeholder", V "placeholder"]
                                  , Nop                 ⤳ S ["2" `Rename` "num_topics", V "in offset", V "in size", V "topic1", V "topic2", "placeholder" `Rename` "topic3", "placeholder" `Rename` "topic4"]
                                  , ProcedureCall "log" ⤳ S []
                                  ]
          aux (Op LOG3)         = fromRight . consError "instr LOG3" . checkOps sigs $
                                  [ StateForce          ⤳ S [N "in offset", N "in size", N "topic1", N "topic2", N "topic3", N "placeholder"]
                                  , Push 3              ⤳ S [N "3", V "in offset", V "in size", V "topic1", V "topic2", V "topic3", V "placeholder"]
                                  , Nop                 ⤳ S ["3" `Rename` "num_topics", V "in offset", V "in size", V "topic1", V "topic2", V "topic3", "placeholder" `Rename` "topic4"]
                                  , ProcedureCall "log" ⤳ S []
                                  ]
          aux (Op LOG4)         = fromRight . consError "instr LOG4" . checkOps sigs $
                                  [ StateForce          ⤳ S [N "in offset", N "in size", N "topic1", N "topic2", N "topic3", N "topic4"]
                                  , Push 4              ⤳ S [N "4", V "in offset", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
                                  , Nop                 ⤳ S ["4" `Rename` "num_topics", V "in offset", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
                                  , ProcedureCall "log" ⤳ S []
                                  ]
          aux (Op CALL)         = fromRight . consError "instr CALL" . checkOps sigs $
                                  [ StateForce           ⤳ S [N "gas", N "to", N "value", N "in offset", N "in size", N "out offset", N "out size"]
                                  , ProcedureCall "call" ⤳ S [N "success"]
                                  ]
          aux (Op RETURN)       = fromRight . consError "instr RETURN" . checkOps sigs $
                                  [ StateForce             ⤳ S [N "offset", N "size"]
                                  , ProcedureCall "return" ⤳ Ø
                                  ]
          aux (Op (Unknown _))  = fromRight . consError "instr Unknown" . checkOps sigs $
                                  [ StateForce          ⤳ S []
                                  , ProcedureCall "die" ⤳ Ø
                                  ]
          aux op                = [ op ]
          sigs = signatures $ procs mc





link (InstrumentedContract procs jt contract) = [ ProcedureCall "init" ]
                                                ++ contract
                                                ++ [ Op STOP ]
                                                ++ concatMap aux procs
                                                ++ jt
    where aux (Procedure name _ _ opswithstacks) = map getOp opswithstacks
          getOp (OpcodePlusWithState op stack) = op

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
          aux (_, ProcedureReturn) = [ Op JUMP ]
          aux (_, StateForce)      = []
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

stackChange :: OpcodePlus -> (Int, Int)
stackChange (Op STOP)         = (0, 0)
stackChange (Op ADD)          = (2, 1)
stackChange (Op MUL)          = (2, 1)
stackChange (Op SUB)          = (2, 1)
stackChange (Op DIV)          = (2, 1)
stackChange (Op SDIV)         = (2, 1)
stackChange (Op MOD)          = (2, 1)
stackChange (Op SMOD)         = (2, 1)
stackChange (Op ADDMOD)       = (3, 1)
stackChange (Op MULMOD)       = (3, 1)
stackChange (Op EXP)          = (2, 1)
stackChange (Op SIGNEXTEND)   = (2, 1)
stackChange (Op LT)           = (2, 1)
stackChange (Op GT)           = (2, 1)
stackChange (Op SLT)          = (2, 1)
stackChange (Op SGT)          = (2, 1)
stackChange (Op EQ)           = (2, 1)
stackChange (Op ISZERO)       = (1, 1)
stackChange (Op AND)          = (2, 1)
stackChange (Op OR)           = (2, 1)
stackChange (Op XOR)          = (2, 1)
stackChange (Op NOT)          = (1, 1)
stackChange (Op BYTE)         = (2, 1)
stackChange (Op SHA3)         = (2, 1)
stackChange (Op ADDRESS)      = (0, 1)
stackChange (Op BALANCE)      = (1, 1)
stackChange (Op ORIGIN)       = (0, 1)
stackChange (Op CALLER)       = (0, 1)
stackChange (Op CALLVALUE)    = (0, 1)
stackChange (Op CALLDATALOAD) = (1, 1)
stackChange (Op CALLDATASIZE) = (0, 1)
stackChange (Op CALLDATACOPY) = (3, 0)
stackChange (Op CODESIZE)     = (0, 1)
stackChange (Op CODECOPY)     = (3, 0)
stackChange (Op GASPRICE)     = (0, 1)
stackChange (Op EXTCODESIZE)  = (1, 1)
stackChange (Op EXTCODECOPY)  = (4, 0)
stackChange (Op BLOCKHASH)    = (1, 1)
stackChange (Op COINBASE)     = (0, 1)
stackChange (Op TIMESTAMP)    = (0, 1)
stackChange (Op NUMBER)       = (0, 1)
stackChange (Op DIFFICULTY)   = (0, 1)
stackChange (Op GASLIMIT)     = (0, 1)
stackChange (Op POP)          = (1, 0)
stackChange (Op MLOAD)        = (1, 1)
stackChange (Op MSTORE)       = (2, 0)
stackChange (Op MSTORE8)      = (2, 0)
stackChange (Op SLOAD)        = (1, 1)
stackChange (Op SSTORE)       = (2, 0)
stackChange (Op JUMP)         = (1, 0)
stackChange (Op JUMPI)        = (2, 0)
stackChange (Op PC)           = (0, 1)
stackChange (Op MSIZE)        = (0, 1)
stackChange (Op GAS)          = (0, 1)
stackChange (Op JUMPDEST)     = (0, 0)
stackChange (Op (PUSH _ _))   = (0, 1)
stackChange (Op (DUP _))      = error "shouldn't be called (DUP)"
stackChange (Op (SWAP _))     = error "shouldn't be called (SWAP)"
stackChange (Op LOG0)         = (2, 0)
stackChange (Op LOG1)         = (3, 0)
stackChange (Op LOG2)         = (4, 0)
stackChange (Op LOG3)         = (5, 0)
stackChange (Op LOG4)         = (6, 0)
stackChange (Op CREATE)       = (3, 1)
stackChange (Op CALL)         = (7, 1)
stackChange (Op CALLCODE)     = (7, 1)
stackChange (Op RETURN)       = (2, 0)
stackChange (Op DELEGATECALL) = (6, 1)
stackChange (Op (Unknown _))  = error "shouldn't be called (Unknown)"
stackChange (Op SUICIDE)      = (1, 0)
stackChange (TagJumpdest _)   = (0, 0)
stackChange (TagJump _)       = (0, 0)
stackChange (TagJumpi _)      = (1, 0)
stackChange (TagPush _)       = (0, 1)
stackChange (Push _)          = (0, 1)
stackChange (ProcedureCall _) = error "shouldn't be called (ProcedureCall)"
stackChange ProcedureReturn   = error "shouldn't be called (ProcedureReturn)"
stackChange StateForce        = error "shouldn't be called (StateForce)"
stackChange Nop               = (0, 0)

infixl 0 @!
(@!) p s = if p then Right () else Left s

checkOp :: [(String, ([StackItem], State))] -> OpcodePlus -> State -> State -> Either String ()
checkOp sigs = aux -- TODO(lorenzb): What about TagJumpdest, TagJump, TagJumpi, TagPush
    where aux StateForce        _       _        = Right ()
          aux (Op JUMPDEST)     Ø       post     = Right ()
          aux (TagJumpdest _)   Ø       post     = Right ()
          aux _                 Ø       post     = post == Ø @! "Unreachable op must have Ø post state"
          aux (Op STOP)         _       post     = post == Ø @! "code after STOP unreachable"
          aux (Op JUMP)         (S pre) post     = do 1 <= length pre @! "stack size mismatch"
                                                      post == Ø @! "code after JUMP unreachable"
          aux (Op RETURN)       (S pre) post     = do 2 <= length pre @! "stack size mismatch"
                                                      post == Ø @! "code after RETURN unreachable"
          aux (ProcedureReturn) (S pre) post     = do 1 <= length pre @! "stack size mismatch"
                                                      head pre == Ret @! "ProcedureReturn takes R from stack"
                                                      post == Ø @! "code after ProcedureReturn unreachable"
          aux (Op SUICIDE)      _       post     = post == Ø @! "code after SUICIDE unreachable"
          aux (Op (Unknown _))  _       post     = post == Ø @! "code after Unknown unreachable"
          aux (TagJump _)       _       post     = post == Ø @! "code after TagJump unreachable"
          aux (Op (SWAP n))     (S pre) (S post) = do length pre == length post @! "stack size mismatch"
                                                      let n' = fromIntegral n
                                                      n' <= length pre @! "stack is too small for SWAP"
                                                      [pre !! n'] ++ (drop 1 . take n') pre ++ [pre !! 0] ++ drop (n'+1) pre ≺ post @! "invalid SWAP" ++ show pre
          aux (Op (DUP n))      (S pre) (S post) = do length pre + 1 == length post @! "stack size mismatch"
                                                      let n' = fromIntegral n
                                                      n' <= length pre @! "stack is too small for DUP"
                                                      pre ≺ tail post @! "invalid DUP"
                                                      post !! n' == post !! 0 @! "invalid DUP (wrong top of stack)"
          aux (ProcedureCall p) (S pre) Ø        = do (procPre, procPost) <- failNothing "Call to invalid procedure " $ lookup p sigs
                                                      procPost == Ø @! "ProcedureCall doesn't lead to unreachable state"
          aux (ProcedureCall p) (S pre) (S post) = do (procPre, procPost') <- failNothing "Call to invalid procedure " $ lookup p sigs
                                                      procPost <- case procPost' of
                                                                      S s -> Right s
                                                                      Ø   -> Left "ProcedureCall leads to unreachable state"
                                                      0 <= length pre - length procPre @! "stack too small for procedure call"
                                                      length pre - length procPre == length post - length procPost @! "stack size mismatch"
                                                      take (length procPre) pre ≺ procPre @! "procedure arguments don't match"
                                                      drop (length procPre) pre ≺ drop (length procPost) post @! "rest of stack doesn't match"
                                                      take (length procPost) post ≺ procPost @! "procedure returns don't match"
                                                      all (\s -> case s of {(N _) -> True; _ -> False}) (take (length procPost) post) @! "procedure return not marked as N"
          aux op                (S pre) (S post) = do let (delete, add) = stackChange op
                                                      0 <= length pre - delete @! "stack too small"
                                                      length pre - delete == length post - add @! "stack size mismatch"
                                                      drop delete pre ≺ drop add post @! "rest of stack doesn't match"
                                                      all (\s -> case s of {(N _) -> True; _ -> False}) (take add post) @! "op returns not marked as N"
          aux op                (S pre) Ø        = Left $ show op ++ " never introduces unreachable state"


pairs :: [a] -> [(a,a)]
pairs (x:x':xs) = (x, x') : pairs (x':xs)
pairs [_]       = []
pairs []        = []

triplets :: [OpcodePlusWithState] -> [(OpcodePlus, State, State)]
triplets = map (\(OpcodePlusWithState _ pre, OpcodePlusWithState op post) -> (op, pre, post)) . pairs

signatures :: [Procedure] -> [(String, ([StackItem], State))]
signatures = checkKeys . map (\(Procedure name pre post _) -> (name, (pre, post)))

checkOps :: [(String, ([StackItem], State))] -> [OpcodePlusWithState] -> Either (Int, String) [OpcodePlus]
checkOps sigs ops = checkStart >> checkStraight >> Right (map (\(OpcodePlusWithState op _) -> op) ops)
    where checkStart = case ops of
                           (OpcodePlusWithState StateForce _: _) -> Right ()
                           _                                     -> Left (0, "Must start with StateForce")
          checkStraight = sequence_ $ (zipWith consError) [1..] $ map (uncurry3 $ checkOp sigs) $ triplets ops

-- TODO (lorenzb): Get rid of this
uncurry3 f (x,y,z) = f x y z


checkProcedure :: [Procedure] -> Procedure -> Either (String, (Int, String)) ()
checkProcedure ps (Procedure name pre post ops) = consError name $ do checkStart
                                                                      checkStraight
                                                                      checkJumps
                                                                      checkUnreachableReturn
                                                                      checkReturns
    where sigs = signatures ps
          ops' = triplets ops
          checkStart = case ops of
                           []     -> Left (0, "Procedure must not be empty")
                           (op:_) -> op == (TagJumpdest name ⤳ S (Ret : pre)) @! (0, "Invalid TagJumpdest")
          checkStraight = sequence_ $ zipWith consError [1..] $ map (uncurry3 $ checkOp sigs) ops'
          checkJumps = sequence_ $ zipWith consError [1..] $ map aux ops'
              where aux ((TagJumpdest tag), _, S post) = do pres <- jumpPres tag
                                                            sequence_ [pre ≺ post @! "bad jump with tag " ++ tag | pre <- pres]
                    aux ((TagJumpdest tag), _, _)      = Left "fuck"
                    aux (_                , _, _)      = Right ()
          jumpPres tag = sequence $ mapMaybe aux ops'
              where aux ((TagJump tag') , S pre, _) = if tag' == tag then Just $ Right pre else Nothing
                    aux ((TagJumpi tag'), S pre, _) = if tag' == tag then Just $ case pre of {(cond:pre') -> Right pre'; [] -> Left "Invalid TagJumpi"} else Nothing
                    aux (_              ,   _,   _) = Nothing
          checkUnreachableReturn = when (post == Ø) $ sequence_ $ zipWith consError [1..] $ map aux ops'
              where aux (ProcedureReturn, S pre, _) = Left "Invalid Procedure return value Ø"
                    aux _                           = Right ()
          checkReturns = sequence_ $ zipWith consError [1..] $ map aux ops'
              where aux (ProcedureReturn, S pre, _) = case (pre, post) of --TODO(lorenzb): procedure can return Ø
                                                          ((Ret:pre'), S post') -> pre' ≺ post' @! "Invalid ProcedureReturn"
                                                          _                     -> Left "Wrong stack for ProcedureReturn"
                    aux _                           = Right ()

infix 1 ≺
class StackOrd a where
    (≺) :: a -> a -> Bool

instance StackOrd StackItem where
    (≺) (N x)        (V x')       = x == x'
    (≺) (V x)        (V x')       = x == x'
    (≺) Ret          Ret          = True
    (≺) i            (Rename x _) = i ≺ V x
    (≺) (Rename _ x) i            = V x ≺ i
    (≺) _            _            = False

instance (StackOrd a) => StackOrd [a] where
    (≺) pre post = length pre == length post && and (zipWith (≺) pre post)

instance StackOrd State where
    (≺) (S pre) (S post) = pre ≺ post
    (≺) _       _        = False

procs :: Integer -> [Procedure]
procs mc = [ procBalance
           , procCall
           , procDie
           , procInit
           , procMcAddress mc
           , procMemcpyExcl
           , procLog
           , procReturn
           ]

memoryStashSize = 9 * 0x20

calldataStashSize = 2 * 0x20

maxPrecompileAddress = 16

procLog :: Procedure
procLog = Procedure "log" [V "num_topics", V "in offset", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"] (S [])
    [ TagJumpdest "log"             ⤳ S [Ret, V "num_topics", V "in offset", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    -- adjust offset by memoryStashSize
    , SWAP 2                        ⤳ S [V "in offset", V "num_topics", Ret, V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push memoryStashSize          ⤳ S [N "memoryStashSize", V "in offset", V "num_topics", Ret, V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ADD                           ⤳ S [N "in offset'", V "num_topics", Ret, V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , SWAP 2                        ⤳ S [Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    -- back up memory
    , DUP 2                         ⤳ S [V "num_topics", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push 1                        ⤳ S [N "1", V "num_topics",  Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ADD                           ⤳ S [N "num_topics+1", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push 0x20                     ⤳ S [N "0x20", V "num_topics+1", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , MUL                           ⤳ S [N "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push 0                        ⤳ S [N "0", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 1                         ⤳ S [V "0", V "0", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 3                         ⤳ S [V "(num_topics+1)*0x20", V "0", V "0", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Nop                           ⤳ S [V "(num_topics+1)*0x20", "0" `Rename` "excl_dst", "0" `Rename` "excl_dst_size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 8                         ⤳ S [V "in size", V "(num_topics+1)*0x20", V "excl_dst", V "excl_dst_size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 8                         ⤳ S [V "in offset'", V "in size", V "(num_topics+1)*0x20", V "excl_dst", V "excl_dst_size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ADD                           ⤳ S [N "in offset' + in size", V "(num_topics+1)*0x20", V "excl_dst", V "excl_dst_size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push 0                        ⤳ S [N "0", V "in offset' + in size", V "(num_topics+1)*0x20", V "excl_dst", V "excl_dst_size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Nop                           ⤳ S ["0" `Rename` "dst", "in offset' + in size" `Rename` "src", "(num_topics+1)*0x20" `Rename` "size", V "excl_dst", V "excl_dst_size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ProcedureCall "memcpy_excl"   ⤳ S [V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 5                         ⤳ S [V "in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 5                         ⤳ S [V "in offset'", V "in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ADD                           ⤳ S [N "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    -- first topic
    , DUP 4                         ⤳ S [V "num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push 1                        ⤳ S [N "1", V "num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , GT                            ⤳ S [N "1 > num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , TagJumpi "no_more_topics"     ⤳ S [V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 7                         ⤳ S [V "topic1", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 2                         ⤳ S [V "in offset' + in size", V "topic1", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , MSTORE                        ⤳ S [V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    -- second topic (30)
    , DUP 4                         ⤳ S [V "num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push 2                        ⤳ S [N "2", V "num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , GT                            ⤳ S [N "2 > num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , TagJumpi "no_more_topics"     ⤳ S [V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 8                         ⤳ S [V "topic2", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 2                         ⤳ S [V "in offset' + in size", V "topic2", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push (1 * 0x20)               ⤳ S [N "1 * 0x20", V "in offset' + in size", V "topic2", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ADD                           ⤳ S [N "in offset' + in size + 1 * 0x20", V "topic2", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , MSTORE                        ⤳ S [V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    -- third topic (39)
    , DUP 4                         ⤳ S [V "num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push 3                        ⤳ S [N "3", V "num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , GT                            ⤳ S [N "3 > num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , TagJumpi "no_more_topics"     ⤳ S [V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 9                         ⤳ S [V "topic3", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 2                         ⤳ S [V "in offset' + in size", V "topic3", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push (2 * 0x20)               ⤳ S [N "2 * 0x20", V "in offset' + in size", V "topic3", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ADD                           ⤳ S [N "in offset' + in size + 2 * 0x20", V "topic3", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , MSTORE                        ⤳ S [V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    -- fourth topic (48)
    , DUP 4                         ⤳ S [V "num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push 4                        ⤳ S [N "4", V "num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , GT                            ⤳ S [N "4 > num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , TagJumpi "no_more_topics"     ⤳ S [V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 10                        ⤳ S [V "topic4", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 2                         ⤳ S [V "in offset' + in size", V "topic4", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push (3 * 0x20)               ⤳ S [N "3 * 0x20", V "in offset' + in size", V "topic4", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ADD                           ⤳ S [N "in offset' + in size + 3 * 0x20", V "topic4", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , MSTORE                        ⤳ S [V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    -- stored all topics in memory
    , TagJumpdest "no_more_topics"  ⤳ S [V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 2                         ⤳ S [V "(num_topics+1)*0x20", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , SWAP 1                        ⤳ S [V "in offset' + in size", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    -- store number of topics in memory
    , DUP 2                         ⤳ S [V "(num_topics+1)*0x20", V "in offset' + in size", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ADD                           ⤳ S [N "in offset' + in size + (num_topics+1)*0x20", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push 0x20                     ⤳ S [N "0x20", V "in offset' + in size + (num_topics+1)*0x20", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , SWAP 1                        ⤳ S [V "in offset' + in size + (num_topics+1)*0x20", V "0x20", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , SUB                           ⤳ S [N "in offset' + in size + num_topics*0x20", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 5                         ⤳ S [V "num_topics", V "in offset' + in size + num_topics*0x20", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , SWAP 1                        ⤳ S [V "in offset' + in size + num_topics*0x20", V "num_topics", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , MSTORE                        ⤳ S [V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    -- CALL
    , DUP 6                         ⤳ S [V "in size", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ADD                           ⤳ S [N "in size + (num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 1                         ⤳ S [V "in size + (num_topics+1)*0x20", V "in size + (num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 1                         ⤳ S [V "in size + (num_topics+1)*0x20", V "in size + (num_topics+1)*0x20", V "in size + (num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Nop                           ⤳ S [V "in size + (num_topics+1)*0x20", "in size + (num_topics+1)*0x20" `Rename` "empty output", "in size + (num_topics+1)*0x20" `Rename` "empty output", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 7                         ⤳ S [V "in offset'", V "in size + (num_topics+1)*0x20", V "empty output", V "empty output", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push 0                        ⤳ S [N "0", V "in offset'", V "in size + (num_topics+1)*0x20", V "empty output", V "empty output", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ProcedureCall "mc_address"    ⤳ S [N "mc_address", V "0", V "in offset'", V "in size + (num_topics+1)*0x20", V "empty output", V "empty output", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , GAS                           ⤳ S [N "gas", V "mc_address", V "0", V "in offset'", V "in size + (num_topics+1)*0x20", V "empty output", V "empty output", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , CALL                          ⤳ S [N "success", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ISZERO                        ⤳ S [N "fail", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , PC                            ⤳ S [N "invalid jump destination", V "fail", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , JUMPI                         ⤳ S [V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    -- restore memory from backup
    , Push 0                        ⤳ S [N "0", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , SWAP 1                        ⤳ S [V "(num_topics+1)*0x20", V "0", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 1                         ⤳ S [V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", V "0", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 3                         ⤳ S [V "0", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", V "0", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 8                         ⤳ S [V "in size", V "0", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", V "0", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 8                         ⤳ S [V "in offset'", V "in size", V "0", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", V "0", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ADD                           ⤳ S [N "in offset' + in size", V "0", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", V "0", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Nop                           ⤳ S ["in offset' + in size" `Rename` "dst", "0" `Rename` "src", "(num_topics+1)*0x20" `Rename` "size", "(num_topics+1)*0x20" `Rename` "excl_dst", "0" `Rename` "excl_dst_size", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ProcedureCall "memcpy_excl"   ⤳ S [Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    -- clean stack
    , SWAP 3                        ⤳ S [V "in size", V "num_topics", V "in offset'", Ret, V "topic1", V "topic2", V "topic3", V "topic4"]
    , POP                           ⤳ S [V "num_topics", V "in offset'", Ret, V "topic1", V "topic2", V "topic3", V "topic4"]
    , SWAP 1                        ⤳ S [V "in offset'", V "num_topics", Ret, V "topic1", V "topic2", V "topic3", V "topic4"]
    , POP                           ⤳ S [V "num_topics", Ret, V "topic1", V "topic2", V "topic3", V "topic4"]
    -- first item
    , DUP 1                         ⤳ S [V "num_topics", V "num_topics", Ret, V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push 1                        ⤳ S [N "1", V "num_topics", V "num_topics", Ret, V "topic1", V "topic2", V "topic3", V "topic4"]
    , GT                            ⤳ S [N "1 > num_topics", V "num_topics", Ret, V "topic1", V "topic2", V "topic3", V "topic4"]
    , StateForce                    ⤳ S [N "1 > num_topics", V "num_topics", Ret]
    , TagJumpi "no_more_topics2"    ⤳ S [V "num_topics", Ret]
    , StateForce                    ⤳ S [V "num_topics", Ret, V "topic1", V "topic2", V "topic3", V "topic4"]
    , SWAP 2                        ⤳ S [V "topic1", Ret, V "num_topics", V "topic2", V "topic3", V "topic4"]
    , POP                           ⤳ S [Ret, V "num_topics", V "topic2", V "topic3", V "topic4"]
    , SWAP 1                        ⤳ S [V "num_topics", Ret, V "topic2", V "topic3", V "topic4"]
    -- second item
    , DUP 1                         ⤳ S [V "num_topics", V "num_topics", Ret, V "topic2", V "topic3", V "topic4"]
    , Push 2                        ⤳ S [N "2", V "num_topics", V "num_topics", Ret, V "topic2", V "topic3", V "topic4"]
    , GT                            ⤳ S [N "2 > num_topics", V "num_topics", Ret, V "topic2", V "topic3", V "topic4"]
    , StateForce                    ⤳ S [N "2 > num_topics", V "num_topics", Ret]
    , TagJumpi "no_more_topics2"    ⤳ S [V "num_topics", Ret]
    , StateForce                    ⤳ S [V "num_topics", Ret, V "topic2", V "topic3", V "topic4"]
    , SWAP 2                        ⤳ S [V "topic2", Ret, V "num_topics", V "topic3", V "topic4"]
    , POP                           ⤳ S [Ret, V "num_topics", V "topic3", V "topic4"]
    , SWAP 1                        ⤳ S [V "num_topics", Ret, V "topic3", V "topic4"]
    -- third item
    , DUP 1                         ⤳ S [V "num_topics", V "num_topics", Ret, V "topic3", V "topic4"]
    , Push 3                        ⤳ S [N "3", V "num_topics", V "num_topics", Ret, V "topic3", V "topic4"]
    , GT                            ⤳ S [N "3 > num_topics", V "num_topics", Ret, V "topic3", V "topic4"]
    , StateForce                    ⤳ S [N "3 > num_topics", V "num_topics", Ret]
    , TagJumpi "no_more_topics2"    ⤳ S [V "num_topics", Ret]
    , StateForce                    ⤳ S [V "num_topics", Ret, V "topic3", V "topic4"]
    , SWAP 2                        ⤳ S [V "topic3", Ret, V "num_topics", V "topic4"]
    , POP                           ⤳ S [Ret, V "num_topics", V "topic4"]
    , SWAP 1                        ⤳ S [V "num_topics", Ret, V "topic4"]
    -- fourth item
    , DUP 1                         ⤳ S [V "num_topics", V "num_topics", Ret, V "topic4"]
    , Push 4                        ⤳ S [N "4", V "num_topics", V "num_topics", Ret, V "topic4"]
    , GT                            ⤳ S [N "4 > num_topics", V "num_topics", Ret, V "topic4"]
    , StateForce                    ⤳ S [N "4 > num_topics", V "num_topics", Ret]
    , TagJumpi "no_more_topics2"    ⤳ S [V "num_topics", Ret]
    , StateForce                    ⤳ S [V "num_topics", Ret, V "topic4"]
    , SWAP 2                        ⤳ S [V "topic4", Ret, V "num_topics"]
    , POP                           ⤳ S [Ret, V "num_topics"]
    , SWAP 1                        ⤳ S [V "num_topics", Ret]
    -- stack cleaned
    , TagJumpdest "no_more_topics2" ⤳ S [V "num_topics", Ret]
    , POP                           ⤳ S [Ret]
    , ProcedureReturn               ⤳ Ø
    ]

-- calling convention stuff: when calling, prepend calldata with own (not MC's!) address and callvalue
--                           upon return, skip first word of return data. If it returns 0xffff..ffff, also return 0xfff.fff
--                           Otherwise, skip first word, treat rest as output data. If call fails, also fail call.

check :: a -> Bool -> a
check x b = if b then x else error "check failure"

procCall :: Procedure
procCall = Procedure "call" [V "gas", V "to", V "value", V "in offset", V "in size", V "out offset", V "out size"] (S [V "success"]) $
    [ TagJumpdest "call"          ⤳ S [Ret, V "gas", V "to", V "value", V "in offset", V "in size", V "out offset", V "out size"]
    -- Is this a call to self?
    -- TODO(lorenzb): This might be a good place to check that value doesn't exceed mc.balance. Ask Florian what he thinks
    , ProcedureCall "mc_address"  ⤳ S [N "mc_address", Ret, V "gas", V "to", V "value", V "in offset", V "in size", V "out offset", V "out size"]
    , DUP 4                       ⤳ S [V "to", V "mc_address", Ret, V "gas", V "to", V "value", V "in offset", V "in size", V "out offset", V "out size"]
    , EQ                          ⤳ S [N "to == mc_address", Ret, V "gas", V "to", V "value", V "in offset", V "in size", V "out offset", V "out size"]
    , TagJumpi "internal_call"    ⤳ S [Ret, V "gas", V "to", V "value", V "in offset", V "in size", V "out offset", V "out size"]
    -- Adjust offsets by memoryStashSize
    , SWAP 4                      ⤳ S [V "in offset", V "gas", V "to", V "value", Ret, V "in size", V "out offset", V "out size"]
    , Push memoryStashSize        ⤳ S [N "memory stash size", V "in offset", V "gas", V "to", V "value", Ret, V "in size", V "out offset", V "out size"]
    , ADD                         ⤳ S [N "in offset'", V "gas", V "to", V "value", Ret, V "in size", V "out offset", V "out size"]
    , SWAP 4                      ⤳ S [Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset", V "out size"]
    , SWAP 6                      ⤳ S [V "out offset", V "gas", V "to", V "value", V "in offset'", V "in size", Ret, V "out size"]
    , Push memoryStashSize        ⤳ S [N "memory stash size", V "out offset", V "gas", V "to", V "value", V "in offset'", V "in size", Ret, V "out size"]
    , ADD                         ⤳ S [N "out offset'", V "gas", V "to", V "value", V "in offset'", V "in size", Ret, V "out size"]
    , SWAP 6                      ⤳ S [Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    -- If this is a call to a precompile, that's all we need to do. (assuming that precompiles don't accept value != 0. TODO(lorenzb): Verify this assumption)
    , Push maxPrecompileAddress   ⤳ S [N "maxPrecompileAddress", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 4                       ⤳ S [V "to", V "maxPrecompileAddress", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , GT                          ⤳ S [N "not precompile", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , TagJumpi "regular_call"     ⤳ S [Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0                      ⤳ S [N "0", Ret    , V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , StateForce                  ⤳ S [V "0", V "Ret", V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , MSTORE                      ⤳ S [V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , CALL                        ⤳ S [N "success"]
    , Push 0                      ⤳ S [N "0", V "success"]
    , MLOAD                       ⤳ S [N "Ret", V "success"]
    , StateForce                  ⤳ S [Ret    , V "success"]
    , ProcedureReturn             ⤳ Ø
    , TagJumpdest "regular_call"  ⤳ S [Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    -- Backup parts of output region overwritten by instrumentation to M[0..31]
    , DUP 8                       ⤳ S [V "out size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 8                       ⤳ S [V "out offset'", V "out size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , ADD                         ⤳ S [N "out offset' + out size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , MLOAD                       ⤳ S [N "M[out offset' + out size]", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0                      ⤳ S [N "0", V "M[out offset' + out size]", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , MSTORE                      ⤳ S [Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    -- Backup parts of input region overwritten by instrumentation to M[32..191]
    , Push 0                      ⤳ S [N "0", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 1                       ⤳ S [V "0", V "0", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push (5 * 0x20)             ⤳ S [N "5*0x20", V "0", V "0", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 9                       ⤳ S [V "in size", V "5*0x20", V "0", V "0", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 9                       ⤳ S [V "in offset'", V "in size", V "5*0x20", V "0", V "0", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , ADD                         ⤳ S [N "in offset' + in size", V "5*0x20", V "0", V "0", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0x20                   ⤳ S [N "0x20", V "in offset' + in size", V "5*0x20", V "0", V "0", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , Nop                         ⤳ S ["0x20" `Rename` "dst", "in offset' + in size" `Rename` "src", "5*0x20" `Rename` "size", "0" `Rename` "excl_dst", "0" `Rename` "excl_dst_size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , ProcedureCall "memcpy_excl" ⤳ S [Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    -- Append gas, to, value, out size, 5 to input
    , DUP 6                       ⤳ S [V "in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 6                       ⤳ S [V "in offset'", V "in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , ADD                         ⤳ S [N "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    --   append gas
    , DUP 3                       ⤳ S [V "gas", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 2                       ⤳ S [V "in offset' + in size", V "gas", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , MSTORE                      ⤳ S [V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    --   append to
    , DUP 4                       ⤳ S [V "to", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 2                       ⤳ S [V "in offset' + in size", V "to", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0x20                   ⤳ S [N "0x20", V "in offset' + in size", V "to", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , ADD                         ⤳ S [N "in offset' + in size + 0x20", V "to", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , MSTORE                      ⤳ S [V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    --   append value
    , DUP 5                       ⤳ S [V "value", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 2                       ⤳ S [V "in offset' + in size", V "value", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0x40                   ⤳ S [N "0x40", V "in offset' + in size", V "value", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , ADD                         ⤳ S [N "in offset' + in size + 0x40", V "value", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , MSTORE                      ⤳ S [V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    --   append out size
    , DUP 9                       ⤳ S [V "out size", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 2                       ⤳ S [V "in offset' + in size", V "out size", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0x60                   ⤳ S [N "0x60", V "in offset' + in size", V "out size", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , ADD                         ⤳ S [N "in offset' + in size + 0x60", V "out size", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , MSTORE                      ⤳ S [V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    --   append 5
    , Push 5                      ⤳ S [N "5", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 2                       ⤳ S [V "in offset' + in size", V "5", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0x80                   ⤳ S [N "0x80", V "in offset' + in size", V "5", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , ADD                         ⤳ S [N "in offset' + in size + 0x80", V "5", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , MSTORE                      ⤳ S [V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    -- Clean up stack a little
    , POP                         ⤳ S [Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , SWAP 3                      ⤳ S [V "value", V "gas", V "to", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , POP                         ⤳ S [V "gas", V "to", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , POP                         ⤳ S [V "to", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , POP                         ⤳ S [Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    -- Prepare arguments for CALL, perform CALL, crash on failure
    --   Add 0x20 to outsize (for success)
    , DUP 5                       ⤳ S [V "out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0x20                   ⤳ S [N "0x20", V "out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , ADD                         ⤳ S [N "out size + 0x20", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    --   out offset'
    , DUP 5                       ⤳ S [V "out offset'", V "out size + 0x20", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    --   in size + 0xa0 (for gas, to, value, out size, 5)
    , DUP 5                       ⤳ S [V "in size", V "out offset'", V "out size + 0x20", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push (5 * 0x20)             ⤳ S [N "0xA0", V "in size", V "out offset'", V "out size + 0x20", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , ADD                         ⤳ S [N "in size + 0xA0", V "out offset'", V "out size + 0x20", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    --   in offset
    , DUP 5                       ⤳ S [V "in offset'", V "in size + 0xA0", V "out offset'", V "out size + 0x20", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    --   value = 0
    , Push 0                      ⤳ S [N "0", V "in offset'", V "in size + 0xA0", V "out offset'", V "out size + 0x20", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    --   to = mc
    , ProcedureCall "mc_address"  ⤳ S [N "mc_address", V "0", V "in offset'", V "in size + 0xA0", V "out offset'", V "out size + 0x20", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    --   gas = all remaining gas
    , GAS                         ⤳ S [N "gas", V "mc_address", V "0", V "in offset'", V "in size + 0xA0", V "out offset'", V "out size + 0x20", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    --    CALL
    , CALL                        ⤳ S [N "call succeeded", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    --    crash on failure
    , ISZERO                      ⤳ S [N "call failed", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , PC                          ⤳ S [N "invalid jump destination", V "call failed", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , JUMPI                       ⤳ S [Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    -- Restore output backup and store return value in M[0]
    , DUP 5                       ⤳ S [V "out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 5                       ⤳ S [V "out offset'", V "out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , ADD                         ⤳ S [N "out offset' + out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 1                       ⤳ S [V "out offset' + out size", V "out offset' + out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , MLOAD                       ⤳ S [N "return value", V "out offset' + out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0                      ⤳ S [N "0", V "return value", V "out offset' + out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , MLOAD                       ⤳ S [N "M[0]", V "return value", V "out offset' + out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , SWAP 1                      ⤳ S [V "return value", V "M[0]", V "out offset' + out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0                      ⤳ S [N "0", V "return value", V "M[0]", V "out offset' + out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , MSTORE                      ⤳ S [V "M[0]", V "out offset' + out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , SWAP 1                      ⤳ S [V "out offset' + out size", V "M[0]", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , MSTORE                      ⤳ S [Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    -- Restore input backup whereever it does not overlap with output
    , DUP 5                       ⤳ S [V "out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 5                       ⤳ S [V "out offset'", V "out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0xA0                   ⤳ S [N "0xA0", V "out offset'", V "out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0x20                   ⤳ S [N "0x20", V "0xA0", V "out offset'", V "out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 7                       ⤳ S [V "in size", V "0x20", V "0xA0", V "out offset'", V "out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 7                       ⤳ S [V "in offset'", V "in size", V "0x20", V "0xA0", V "out offset'", V "out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , ADD                         ⤳ S [N "in offset' + in size", V "0x20", V "0xA0", V "out offset'", V "out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , Nop                         ⤳ S ["in offset' + in size" `Rename` "dst", "0x20" `Rename` "src", "0xA0" `Rename` "size", "out offset'" `Rename` "excl_dst", "out size" `Rename` "excl_dst_size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , ProcedureCall "memcpy_excl" ⤳ S [Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    -- Clean up stack
    , SWAP 4                      ⤳ S [V "out size", V "in offset'", V "in size", V "out offset'", Ret]
    , POP                         ⤳ S [V "in offset'", V "in size", V "out offset'", Ret]
    , POP                         ⤳ S [V "in size", V "out offset'", Ret]
    , POP                         ⤳ S [V "out offset'", Ret]
    , POP                         ⤳ S [Ret]
    -- Load success from M[0..31] and return
    , Push 0                      ⤳ S [N "0", Ret]
    , MLOAD                       ⤳ S [N "success", Ret]
    , SWAP 1                      ⤳ S [Ret, V "success"]
    , ProcedureReturn             ⤳ Ø
    ] ++
    let stashRet          = 0 * 0x20
        stashOutputPrefix = 1 * 0x20
        stashInputPrefix1 = 2 * 0x20
        stashInputPrefix2 = 3 * 0x20
        stashInputOffset  = 4 * 0x20
        stashOutputOffset = 5 * 0x20
        stashOutputSize   = 6 * 0x20
        inputPrefixWidth  = 2 * 0x20
        outputPrefixWidth = 1 * 0x20
        -- since we prefix memory (with at most two words), this procedure requires at memoryStashSize >= (6 + 1 + 2) * 0x20
    in
    -----------------
    -- Internal call
    -----------------
    [ TagJumpdest "internal_call"                           ⤳ S [Ret, V "gas", V "to", V "value", V "in offset", V "in size", V "out offset", V "out size"]
    -- We don't call any other procedures that modify M[0] in this procedure, so we can safely store the return pc there
    , StateForce                                            ⤳ S [N "Ret", V "gas", V "to", V "value", V "in offset", V "in size", V "out offset", V "out size"]
    , Push 0                                                ⤳ S [N "0", V "Ret", V "gas", V "to", V "value", V "in offset", V "in size", V "out offset", V "out size"]
    , MSTORE                                                ⤳ S [V "gas", V "to", V "value", V "in offset", V "in size", V "out offset", V "out size"] -- TODO(lorenzb): Should we give internal calls control over gas?
    -- Adjust input and output regions
    , SWAP 3                                                ⤳ S [V "in offset", V "to", V "value", V "gas", V "in size", V "out offset", V "out size"]
    , Push (memoryStashSize - inputPrefixWidth)             ⤳ S [N "memoryStashSize - in pre width", V "in offset", V "to", V "value", V "gas", V "in size", V "out offset", V "out size"]
    , ADD                                                   ⤳ S [N "in offset' - in pre width", V "to", V "value", V "gas", V "in size", V "out offset", V "out size"]
    , SWAP 3                                                ⤳ S [V "gas", V "to", V "value", V "in offset' - in pre width", V "in size", V "out offset", V "out size"]
    , SWAP 4                                                ⤳ S [V "in size", V "to", V "value", V "in offset' - in pre width", V "gas", V "out offset", V "out size"]
    , Push inputPrefixWidth                                 ⤳ S [N "in pre width", V "in size", V "to", V "value", V "in offset' - in pre width", V "gas", V "out offset", V "out size"]
    , ADD                                                   ⤳ S [N "in size + in pre width", V "to", V "value", V "in offset' - in pre width", V "gas", V "out offset", V "out size"]
    , SWAP 4                                                ⤳ S [V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset", V "out size"]
    , SWAP 5                                                ⤳ S [V "out offset", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "gas", V "out size"]
    , Push (memoryStashSize - outputPrefixWidth)            ⤳ S [N "memoryStashSize - out pre width", V "out offset", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "gas", V "out size"]
    , ADD                                                   ⤳ S [N "out offset' - out pre width", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "gas", V "out size"]
    , SWAP 5                                                ⤳ S [V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size"]
    , SWAP 6                                                ⤳ S [V "out size", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "gas"]
    , Push outputPrefixWidth                                ⤳ S [N "out pre width", V "out size", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "gas"]
    , ADD                                                   ⤳ S [N "out size + out pre width", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "gas"]
    , SWAP 6                                                ⤳ S [V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    -- Backup prefix of output region (1 word)
    , DUP 6                                                 ⤳ S [V "out offset' - out pre width", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , MLOAD                                                 ⤳ S [N "M[out offset' - out pre width]", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , Push stashOutputPrefix                                ⤳ S [N "stashOutputPrefix", V "M[out offset' - out pre width]", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , MSTORE                                                ⤳ S [V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    -- Backup prefix of input region (2 words)
    , DUP 4                                                 ⤳ S [V "in offset' - in pre width", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , MLOAD                                                 ⤳ S [N "M[in offset' - in pre width]", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , Push stashInputPrefix1                                ⤳ S [N "stashInputPrefix1", V "M[in offset' - in pre width]", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , MSTORE                                                ⤳ S [V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , DUP 4                                                 ⤳ S [V "in offset' - in pre width", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , Push 0x20                                             ⤳ S [N "0x20", V "in offset' - in pre width", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , ADD                                                   ⤳ S [N "in offset' - in pre width + 0x20", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , MLOAD                                                 ⤳ S [N "M[in offset' - in pre width + 0x20]", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , Push stashInputPrefix2                                ⤳ S [N "stashInputPrefix2", V "M[in offset' - in pre width + 0x20]", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , MSTORE                                                ⤳ S [V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    -- Backup "in offset' - in pre width"
    , DUP 4                                                 ⤳ S [V "in offset' - in pre width", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , Push stashInputOffset                                 ⤳ S [N "stashInputOffset", V "in offset' - in pre width", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , MSTORE                                                ⤳ S [V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    -- Backup "out offset' - out pre width" to M[160..191]
    , DUP 6                                                 ⤳ S [V "out offset' - out pre width", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , Push stashOutputOffset                                ⤳ S [N "stashOutputOffset", V "out offset' - out pre width", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , MSTORE                                                ⤳ S [V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    -- Backup "out size + out pre width" to M[192..223]
    , DUP 7                                                 ⤳ S [V "out size + out pre width", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , Push stashOutputSize                                  ⤳ S [N "stashOutputSize", V "out size + out pre width", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , MSTORE                                                ⤳ S [V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    -- Store address for call data
    , ProcedureCall "mc_address"                            ⤳ S [N "mc_address", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , DUP 5                                                 ⤳ S [V "in offset' - in pre width", V "mc_address", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , MSTORE                                                ⤳ S [V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    -- Check, store, and change call value
    , SWAP 2                                                ⤳ S [V "value", V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , DUP 1                                                 ⤳ S [V "value", V "value", V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , ProcedureCall "mc_address"                            ⤳ S [N "mc_address", V "value", V "value", V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , BALANCE                                               ⤳ S [N "balance", V "value", V "value", V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , LT                                                    ⤳ S [N "balance < value", V "value", V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , PC                                                    ⤳ S [N "invalid jump destination", V "balance < value", V "value", V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , JUMPI                                                 ⤳ S [V "value", V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , DUP 4                                                 ⤳ S [V "in offset' - in pre width", V "value", V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , Push 0x20                                             ⤳ S [N "0x20", V "in offset' - in pre width", V "value", V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , ADD                                                   ⤳ S [N "in offset' - in pre width + 0x20", V "value", V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , MSTORE                                                ⤳ S [V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , Push 0                                                ⤳ S [N "0", V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , SWAP 2                                                ⤳ S [V "gas", V "to", V "0", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    -- Replace to with own address
    , SWAP 1                                                ⤳ S [V "to", V "gas", V "0", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , POP                                                   ⤳ S [V "gas", V "0", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , ADDRESS                                               ⤳ S [N "self address", V "gas", V "0", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , SWAP 1                                                ⤳ S [V "gas", V "self address", V "0", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    -- CALL
    , CALL                                                  ⤳ S [N "success"]
    , DUP 1                                                 ⤳ S [V "success", V "success"]
    , ISZERO                                                ⤳ S [N "fail", V "success"]
    , TagJumpi "skip_check_first_word_of_output"            ⤳ S [V "success"]
    -- Check whether first word of output indicates a throw and restore memory from backup
    , Push stashOutputOffset                                ⤳ S [N "stashOutputOffset", V "success"]
    , MLOAD                                                 ⤳ S [N "out offset' - out pre width", V "success"]
    , MLOAD                                                 ⤳ S [N "M[out offset' - out pre width]", V "success"]
    , NOT                                                   ⤳ S [N "M[out offset' - out pre width] != 0xff..ff", V "success"]
    , TagJumpi "dont_die"                                   ⤳ S [V "success"]
    , ProcedureCall "die"                                   ⤳ Ø
    , TagJumpdest "dont_die"                                ⤳ S [V "success"]
    , Push stashOutputPrefix                                ⤳ S [N "stashOutputPrefix", V "success"]
    , MLOAD                                                 ⤳ S [N "old M[out offset' - out pre width]", V "success"]
    , Push stashOutputOffset                                ⤳ S [N "stashOutputOffset", V "old M[out offset' - out pre width]", V "success"]
    , MLOAD                                                 ⤳ S [N "out offset' - out pre width", V "old M[out offset' - out pre width]", V "success"]
    , MSTORE                                                ⤳ S [V "success"]
    , TagJumpdest "skip_check_first_word_of_output"         ⤳ S [V "success"]
    -- Restore input region
    -- If the CALL failed, we want to restore all memory in the input region, even if it overlapped with the output
    , Push stashOutputSize                                  ⤳ S [N "stashOutputSize", V "success"]
    , MLOAD                                                 ⤳ S [N "out size + out pre width", V "success"]
    , DUP 2                                                 ⤳ S [V "success", V "out size + out pre width", V "success"]
    , MUL                                                   ⤳ S [N "success * (out size + out pre width)", V "success"]
    , Push stashOutputOffset                                ⤳ S [N "stashOutputOffset", V "success * (out size + out pre width)", V "success"]
    , MLOAD                                                 ⤳ S [N "out offset' - out pre width", V "success * (out size + out pre width)", V "success"]
    , Push inputPrefixWidth                                 ⤳ S [N "in pre width", V "out offset' - out pre width", V "success * (out size + out pre width)", V "success"]
    , DUP 1 `check` (stashInputPrefix1 == inputPrefixWidth) ⤳ S [V "in pre width", V "in pre width", V "out offset' - out pre width", V "success * (out size + out pre width)", V "success"]
    , Nop                                                   ⤳ S ["in pre width" `Rename` "stashInputPrefix1", V "in pre width", V "out offset' - out pre width", V "success * (out size + out pre width)", V "success"]
    , Push stashInputOffset                                 ⤳ S [N "stashInputOffset", V "stashInputPrefix1", V "in pre width", V "out offset' - out pre width", V "success * (out size + out pre width)", V "success"]
    , MLOAD                                                 ⤳ S [N "in offset' - in pre width", V "stashInputPrefix1",  V "in pre width", V "out offset' - out pre width", V "success * (out size + out pre width)", V "success"]
    , Nop                                                   ⤳ S ["in offset' - in pre width" `Rename` "dst", "stashInputPrefix1" `Rename` "src", "in pre width" `Rename` "size", "out offset' - out pre width" `Rename` "excl_dst", "success * (out size + out pre width)" `Rename` "excl_dst_size", V "success"]
    , ProcedureCall "memcpy_excl"                           ⤳ S [V "success"]

    -- Done, retrieve return pc from M[0] and return
    , Push 0                                                ⤳ S [N "0", V "success"]
    , MLOAD                                                 ⤳ S [N "Ret", V "success"]
    , StateForce                                            ⤳ S [Ret, V "success"]
    , ProcedureReturn                                       ⤳ Ø
    ]

procBalance = Procedure "balance" [V "address"] (S [V "balance"])
    [ TagJumpdest "balance"       ⤳ S [Ret, V "address"]
    , SWAP 1                      ⤳ S [V "address", Ret]
    , Push 0                      ⤳ S [N "0", V "address", Ret]
    , MSTORE                      ⤳ S [Ret]
    , Push 6                      ⤳ S [N "6", Ret]
    , Push 0x20                   ⤳ S [N "0x20", V "6", Ret]
    , MSTORE                      ⤳ S [Ret]
    , Push 0x20                   ⤳ S [N "out size = 0x20", Ret]
    , Push 0                      ⤳ S [N "out offset = 0", V "out size = 0x20", Ret]
    , Push 0x40                   ⤳ S [N "in size = 0x40", V "out offset = 0", V "out size = 0x20", Ret]
    , Push 0                      ⤳ S [N "in offset = 0", V "in size = 0x40", V "out offset = 0", V "out size = 0x20", Ret]
    , Push 0                      ⤳ S [N "value = 0", V "in offset = 0", V "in size = 0x40", V "out offset = 0", V "out size = 0x20", Ret]
    , ProcedureCall "mc_address"  ⤳ S [N "mc_address", V "value = 0", V "in offset = 0", V "in size = 0x40", V "out offset = 0", V "out size = 0x20", Ret]
    , GAS                         ⤳ S [N "gas", V "mc_address", V "value = 0", V "in offset = 0", V "in size = 0x40", V "out offset = 0", V "out size = 0x20", Ret]
    , CALL                        ⤳ S [N "success", Ret]
    , ISZERO                      ⤳ S [N "fail", Ret]
    , PC                          ⤳ S [N "pc", V "fail", Ret]
    , JUMPI                       ⤳ S [Ret]
    , Push 0                      ⤳ S [N "0", Ret]
    , MLOAD                       ⤳ S [N "balance", Ret]
    , SWAP 1                      ⤳ S [Ret, V "balance"]
    , ProcedureReturn             ⤳ Ø
    ]

procMemcpyExcl = Procedure "memcpy_excl" [V "dst", V "src", V "size", V "excl_dst", V "excl_dst_size"] (S [])
    [ TagJumpdest "memcpy_excl"           ⤳ S [Ret, V "dst", V "src", V "size", V "excl_dst", V "excl_dst_size"]
    , SWAP 3                              ⤳ S [V "size",  V "dst", V "src", Ret, V "excl_dst", V "excl_dst_size"]
    , TagJumpdest "memcpy_excl_loop"      ⤳ S [V "size",  V "dst", V "src", Ret, V "excl_dst", V "excl_dst_size"]
    , DUP 1                               ⤳ S [V "size", V "size",  V "dst", V "src", Ret, V "excl_dst", V "excl_dst_size"]
    , ISZERO                              ⤳ S [N "size == 0", V "size",  V "dst", V "src", Ret, V "excl_dst", V "excl_dst_size"]
    , TagJumpi "memcpy_excl_end"          ⤳ S [V "size",  V "dst", V "src", Ret, V "excl_dst", V "excl_dst_size"]
    , Push 1                              ⤳ S [N "1", V "size",  V "dst", V "src", Ret, V "excl_dst", V "excl_dst_size"]
    , SWAP 1                              ⤳ S [V "size", V "1", V "dst", V "src", Ret, V "excl_dst", V "excl_dst_size"]
    , SUB                                 ⤳ S [N "size - 1", V "dst", V "src", Ret, V "excl_dst", V "excl_dst_size"]
    , SWAP 2                              ⤳ S [V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , DUP 6                               ⤳ S [V "excl_dst_size", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , DUP 6                               ⤳ S [V "excl_dst", V "excl_dst_size", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , ADD                                 ⤳ S [N "excl_dst + excl_dst_size", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , DUP 3                               ⤳ S [V "dst", V "excl_dst + excl_dst_size", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , LT                                  ⤳ S [N "dst < excl_dst + excl_dst_size", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , DUP 6                               ⤳ S [V "excl_dst", V "dst < excl_dst + excl_dst_size", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , DUP 4                               ⤳ S [V "dst", V "excl_dst", V "dst < excl_dst + excl_dst_size", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , LT                                  ⤳ S [N "dst < excl_dst", V "dst < excl_dst + excl_dst_size", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , ISZERO                              ⤳ S [N "dst >= excl_dst", V "dst < excl_dst + excl_dst_size", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , AND                                 ⤳ S [N "dst >= excl_dst & dst < excl_dst + excl_dst_size", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , TagJumpi "memcpy_excl_skip_copy"    ⤳ S [V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , DUP 1                               ⤳ S [V "src", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , MLOAD                               ⤳ S [N "M[src:src+31]", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , Push 0                              ⤳ S [N "0", V "M[src:src+31]", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , BYTE                                ⤳ S [N "M[src]", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , DUP 3                               ⤳ S [V "dst", V "M[src]", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , MSTORE8                             ⤳ S [V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , TagJumpdest "memcpy_excl_skip_copy" ⤳ S [V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , Push 1                              ⤳ S [N "1", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , ADD                                 ⤳ S [N "src + 1", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , SWAP 1                              ⤳ S [V "dst", V "src + 1", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , Push 1                              ⤳ S [N "1", V "dst", V "src + 1", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , ADD                                 ⤳ S [N "dst + 1", V "src + 1", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , SWAP 1                              ⤳ S [V "src + 1", V "dst + 1", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , SWAP 2                              ⤳ S [V "size - 1", V "dst + 1", V "src + 1", Ret, V "excl_dst", V "excl_dst_size"]
    , Nop                                 ⤳ S ["size - 1" `Rename` "size", "dst + 1" `Rename` "dst", "src + 1" `Rename` "src", Ret, V "excl_dst", V "excl_dst_size"]
    , TagJump "memcpy_excl_loop"          ⤳ Ø
    , TagJumpdest "memcpy_excl_end"       ⤳ S [V "size", V "dst", V "src", Ret, V "excl_dst", V "excl_dst_size"]
    , POP                                 ⤳ S [V "dst", V "src", Ret, V "excl_dst", V "excl_dst_size"]
    , POP                                 ⤳ S [V "src", Ret, V "excl_dst", V "excl_dst_size"]
    , POP                                 ⤳ S [Ret, V "excl_dst", V "excl_dst_size"]
    , SWAP 2                              ⤳ S [V "excl_dst_size", V "excl_dst", Ret]
    , POP                                 ⤳ S [V "excl_dst", Ret]
    , POP                                 ⤳ S [Ret]
    , ProcedureReturn                     ⤳ Ø
    ]

procReturn = Procedure "return" [V "offset", V "size"] Ø
    [ TagJumpdest "return"          ⤳ S [Ret, V "offset", V "size"]
    , POP                           ⤳ S [V "offset", V "size"]
    , Push (memoryStashSize - 0x20) ⤳ S [N "memory stash size - 0x20", V "offset", V "size"]
    , ADD                           ⤳ S [N "offset + memory stash size - 0x20", V "size"]
    , DUP 2                         ⤳ S [V "size", V "offset + memory stash size - 0x20", V "size"]
    , DUP 2                         ⤳ S [V "offset + memory stash size - 0x20", V "size", V "offset + memory stash size - 0x20", V "size"]
    , MSTORE                        ⤳ S [V "offset + memory stash size - 0x20", V "size"]
    , SWAP 1                        ⤳ S [V "size", V "offset + memory stash size - 0x20"]
    , Push 0x20                     ⤳ S [N "0x20", V "size", V "offset + memory stash size - 0x20"]
    , ADD                           ⤳ S [N "size + 0x20", V "offset + memory stash size - 0x20"]
    , SWAP 1                        ⤳ S [V "offset + memory stash size - 0x20", V "size + 0x20"]
    , RETURN                        ⤳ Ø
    ]

procInit :: Procedure
procInit = Procedure "init" [] (S [])
    [ TagJumpdest "init"            ⤳ S [Ret]
    -- Require that caller is mc or self (vyper uses CALL even for contract-internal calls)
    , CALLER                        ⤳ S [N "caller", Ret]
    , ProcedureCall "mc_address"    ⤳ S [N "mc_address", V "caller", Ret]
    , EQ                            ⤳ S [N "mc == caller", Ret]
    , ADDRESS                       ⤳ S [N "self", V "mc == caller", Ret]
    , CALLER                        ⤳ S [N "caller", V "self", V "mc == caller", Ret]
    , EQ                            ⤳ S [N "self == caller", V "mc == caller", Ret]
    , OR                            ⤳ S [N "self == caller | mc == caller", Ret]
    , ISZERO                        ⤳ S [N "!(self == caller | mc == caller)", Ret]
    , PC                            ⤳ S [N "invalid jump destination", V "!(self == caller | mc == caller)", Ret]
    , JUMPI                         ⤳ S [Ret]
    -- Require that call value is 0
    , CALLVALUE                     ⤳ S [N "callvalue", Ret]
    , PC                            ⤳ S [N "invalid jump destination", V "callvalue", Ret]
    , JUMPI                         ⤳ S [Ret]
    -- Require that call data size is at least 2 evm words
    , Push 0x40                     ⤳ S [N "0x40", Ret]
    , CALLDATASIZE                  ⤳ S [N "call data size", V "0x40", Ret]
    , LT                            ⤳ S [N "call data size < 0x40", Ret]
    , PC                            ⤳ S [N "invalid jump destination", V "call data size < 0x40", Ret]
    , JUMPI                         ⤳ S [Ret]
    -- Write to last word of memoryStash so that MSIZE never underflows.
    , Push (memoryStashSize - 0x20) ⤳ S [N "offset of last word of stash", Ret]
    , DUP 1                         ⤳ S [V "offset of last word of stash", V "offset of last word of stash", Ret]
    , MSTORE                        ⤳ S [Ret]
    , ProcedureReturn               ⤳ Ø
    ]

procDie :: Procedure
procDie = Procedure "die" [] Ø
    [ TagJumpdest "die" ⤳ S [Ret]
    , Push 0x20         ⤳ S [N "0x20", Ret]
    , Push 0            ⤳ S [N "0", V "0x20", Ret]
    , DUP 1             ⤳ S [V "0", V"0", V "0x20", Ret]
    , NOT               ⤳ S [N "2^256-1", V"0", V "0x20", Ret]
    , DUP 2             ⤳ S [V "0", V "2^256-1", V"0", V "0x20", Ret]
    , MSTORE            ⤳ S [V"0", V "0x20", Ret]
    , RETURN            ⤳ Ø
    ]

procMcAddress :: Integer -> Procedure
procMcAddress mc = Procedure "mc_address" [] (S [V "mc_address"])
    [ TagJumpdest "mc_address" ⤳ S [Ret]
    , Push mc                  ⤳ S [N "mc_address", Ret]
    , SWAP 1                   ⤳ S [Ret, V "mc_address"]
    , ProcedureReturn          ⤳ Ø
    ]

genericInitcode :: [Opcode]
genericInitcode = fromRight . consError "generic initcode" $
                  do let size = 15
                     opsPlus <- showError . checkOps [] $
                                [ StateForce  ⤳ S []
                                , PC          ⤳ S [N "0"]
                                , CALLVALUE   ⤳ S [N "value", V "0"]
                                , PC          ⤳ S [N "PC", V "value", V "0"]
                                , JUMPI       ⤳ S [V "0"]
                                , PUSH 1 size ⤳ S [N "header size", V "0"]
                                , DUP 1       ⤳ S [V "header size", V "header size", V "0"]
                                , CODESIZE    ⤳ S [N "code size", V "header size", V "header size", V "0"]
                                , SUB         ⤳ S [N "payload size", V "header size", V "0"]
                                , DUP 1       ⤳ S [V "payload size", V "payload size", V "header size", V "0"]
                                , DUP 3       ⤳ S [V "header size", V "payload size", V "payload size", V "header size", V "0"]
                                , DUP 5       ⤳ S [V "0", V "header size", V "payload size", V "payload size", V "header size", V "0"]
                                , CODECOPY    ⤳ S [V "payload size", V "header size", V "0"]
                                , DUP 3       ⤳ S [V "0", V "payload size", V "header size", V "0"]
                                , RETURN      ⤳ Ø
                                ]
                     ops <- lower opsPlus
                     let assembledSize = fromIntegral . B.length . assemble $ ops
                     when (assembledSize /= size) $ Left $ "wrong size:" ++ show assembledSize
                     return ops
