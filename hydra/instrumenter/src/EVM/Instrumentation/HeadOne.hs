module EVM.Instrumentation.HeadOne
where

import           Data.List
import           EVM.Bytecode
import           EVM.BytecodePlus
import           EVM.Instrumentation.Common
import           EVM.While
import qualified EVM.While.Macros as M
import           Prelude          hiding (EQ, GT, LT)
import           Text.Printf
import           Util


-- M[0x00] contains location where actual memory starts mem_start
-- i.e. M[mem_start] contains the first byte of actual memory
-- M[0x100] contains the size of the trace

instrumentOps :: Integer -> [OpcodePlus] -> [OpcodePlus]
instrumentOps mc = concatMap aux
    where aux (Op STOP)         = [ Push 0, Op $ DUP 1, Push 1
                                  -- [success == 1, 0, 0]
                                  , Push 314159265358979, Op $ POP
                                  , ProcedureCall $ procTag "done"
                                  -- Control never reaches this point
                                  ]
          aux (Op ADDRESS)      = [ Push mc    -- ⤳ S [N "mc_address"]
                                  ]
          aux (Op BALANCE)      = [ ProcedureCall $ procTag "balance" -- ⤳ S [N "balance"]
                                  ]
          aux (Op CALLER)       = [ Push 0x00           -- ⤳ S [N "0"]
                                  , Op $ CALLDATALOAD     -- ⤳ S [N "calldata[0]"]
                                  ]
          aux (Op CALLVALUE)    = [ Push 0x20       -- ⤳ S [N "0x20"]
                                  , Op $ CALLDATALOAD    -- ⤳ S [N "calldata[0x20]"]
                                  ]
          -- TODO(lorenzb): CALLDATALOAD, CALLDATASIZE, CALLDATACOPY are vulnerable to overflow
          aux (Op CALLDATALOAD) = [ Push 0x60 -- ⤳ S [N "calldata stash size", V "offset"]
                                  , Op $ ADD                     -- ⤳ S [N "offset + calldata stash size"]
                                  , Op $ CALLDATALOAD            -- ⤳ S [N "C[offset + calldata stash size]"]
                                  ]
          aux (Op CALLDATASIZE) = [ Op $ CALLDATASIZE           -- ⤳ S [N "calldata size"]
                                  , Push 0x60 -- ⤳ S [N "calldata stash size", V "calldata size"]
                                  , Op $ (SWAP  1)               -- ⤳ S [V "calldata size", V "calldata stash size"]
                                  , Op $ SUB                     -- ⤳ S [N "calldata size - calldata stash size"]
                                  ]
          aux (Op CALLDATACOPY) = [ ProcedureCall $ procTag "calldatacopy"
                                  , Op $ POP
                                  ]
          -- TODO(lorenzb): All ops that offset memory are vulnerable to overflow (MLOAD, MSTORE, MSTORE8, CALLDATALOAD, ...)
          aux (Op MLOAD)        = [ ProcedureCall $ procTag "offsetMem"
                                  -- [mem_start + offset]
                                  , Op $ MLOAD
                                  -- [M[mem_start + offset]]
                                  ]
          aux (Op MSTORE)       = [ ProcedureCall $ procTag "offsetMem"
                                  -- [mem_start + offset, word]
                                  , Op $ MSTORE
                                  -- []
                                  ]
          aux (Op MSTORE8)      = [ ProcedureCall $ procTag "offsetMem"
                                  -- [mem_start + offset, byte]
                                  , Op $ MSTORE8
                                  -- []
                                  ]
          aux (Op SHA3)         = [ ProcedureCall $ procTag "sha3"
                                  ]
          aux (Op JUMP)         = [ TagJump "jumptable"
                                  ]
          aux (Op JUMPI)        = [ Op $ (SWAP 1)
                                  -- [should_jump, jumpdest]
                                  , TagJumpi "jumptable" -- ⤳ S [V "pc"]
                                  -- [jumpdest]
                                  , Op $ POP
                                  -- []
                                  ]
          aux (Op MSIZE)        = [ Push memoryMOffset
                                  -- [mem_start]
                                  , Op $ MSIZE
                                  -- [msize, mem_start]
                                  , Op $ SUB
                                  -- [msize - mem_start]
                                  ]
          aux (Op LOG0)         = [ ProcedureCall $ procTag "log0"
                                  , Op POP
                                  ]
          aux (Op LOG1)         = [ ProcedureCall $ procTag "log1"
                                  , Op POP
                                  ]
          aux (Op LOG2)         = [ ProcedureCall $ procTag "log2"
                                  , Op POP
                                  ]
          aux (Op LOG3)         = [ ProcedureCall $ procTag "log3"
                                  , Op POP
                                  ]
          aux (Op LOG4)         = [ ProcedureCall $ procTag "log4"
                                  , Op POP
                                  ]
          aux (Op CALL)         = [ ProcedureCall $ procTag "call"
                                  -- [success]
                                  ]
          aux (Op RETURN)       = [ Push 1
                                  -- [success == 1, offset, size]
                                  , ProcedureCall $ procTag "done"
                                  -- Control never reaches this point
                                  ]
          aux (Op REVERT)       = [ Push 0
                                  -- [success == 0, offset, size]
                                  , ProcedureCall $ procTag "done"
                                  -- Control never reaches this point
                                  ]
          aux (Op (Unknown _))  = [ Push 0, Op $ DUP 1, Op $ DUP 1
                                  -- [success == 0, offset == 0, size = 0]
                                  ,ProcedureCall $ procTag "done"
                                  -- Control never reaches this point
                                  ]
          aux op                = [ op ]


-- TODO(lorenzb): fix order
procs :: Integer -> [Proc]
procs mc = [ procMemcpyPrecomp
           , procMemcpyNoalias
           , procMin
           , procSha3
           , procLog0
           , procLog1
           , procLog2
           , procLog3
           , procLog4
           , procLog
           , procCall
           , procBalance
           , procDone
           , procCalldatacopy
           , procInit
           , procMc mc
           , procUnknownJumpdest
           , procReturndataload
           , procCallHead
           , procOffsetMem memoryMOffset
           ]

backupMOffset = 0x00
backupMSize = 0x20 * 5
tracePtrMOffset = backupMOffset + backupMSize
traceMOffset = tracePtrMOffset + 0x20
traceMSize = 0x20 * 200
memoryMOffset = traceMOffset + traceMSize

getTracePtr = (Mload (Lit tracePtrMOffset))

setTracePtr e = Nest (Scope [(Mstore (Lit tracePtrMOffset) e)
                            ,(assert (M.leq traceSize (Lit traceMSize)))])

traceSize = (Sub getTracePtr (Lit traceMOffset))

backup e1 e2 = (memcpyNoalias (Lit backupMOffset) e1 e2)

restore e1 e2 = (memcpyNoalias e1 (Lit backupMOffset) e2)

-- memoryStashSize = 0x20 * 200

-- backupOffset = 0x20

-- maxBackupSize = traceOffset - backupOffset

-- traceOffset = 0x20 * 6

-- maxTraceSize = memoryStashSize - traceOffset

assert e = M.if_ (Iszero e) (Scope [Revert (Lit 0x00) (Lit 0x00)])
callMc e1 e2 e3 e4 = (Call Gas (ProcCall "mc" []) (Lit 0) e1 e2 e3 e4)

procInit = Proc "init" [] "_" (Scope
           [(checkOrDie (M.and3 (Or (Eq Caller (ProcCall "mc" [])) (Eq Caller Address))
                                (Iszero Callvalue)
                                (Gt Calldatasize (Lit (0x40-1)))))
           ,(checkOrErr errorIncorrectCalldataSize (Eq (Sub Calldatasize (Lit 0x60)) (Calldataload (Lit 0x40))))
           ,(Mstore (Lit (memoryMOffset - 0x20)) (Lit 1))
           ,(setTracePtr (Lit traceMOffset))])

procCalldatacopy = Proc "calldatacopy" ["dst", "src", "size"] "_" (Scope
                   [(Discard (Lit 314159265358979)),(Calldatacopy (Add (Var "dst") (Lit memoryMOffset)) (Add (Var "src") (Lit 0x60)) (Var "size"))])

procLog = Proc "log" ["num_topics", "in_offset", "in_size", "topic1", "topic2", "topic3", "topic4"] "_" (Scope
          [(Assign "in_offset" (Add (Var "in_offset") (Lit memoryMOffset)))
          ,(Let "record_ptr" getTracePtr)
          -- back up words following input
          ,(backup (Add (Var "in_offset") (Var "in_size"))
                   (Mul (Add (Var "num_topics") (Lit 1)) (Lit 0x20)))
          -- append to input: [topic1 .. topicn]
          ,(M.if_ (Lt (Lit 0) (Var "num_topics")) (Scope [(Mstore (M.add3 (Var "in_offset") (Var "in_size") (Lit 0x00)) (Var "topic1"))]))
          ,(M.if_ (Lt (Lit 1) (Var "num_topics")) (Scope [(Mstore (M.add3 (Var "in_offset") (Var "in_size") (Lit 0x20)) (Var "topic2"))]))
          ,(M.if_ (Lt (Lit 2) (Var "num_topics")) (Scope [(Mstore (M.add3 (Var "in_offset") (Var "in_size") (Lit 0x40)) (Var "topic3"))]))
          ,(M.if_ (Lt (Lit 3) (Var "num_topics")) (Scope [(Mstore (M.add3 (Var "in_offset") (Var "in_size") (Lit 0x60)) (Var "topic4"))]))
          -- append to input: num_topics
          ,(Mstore (M.add3 (Var "in_offset") (Var "in_size") (Mul (Var "num_topics") (Lit 0x20))) (Var "num_topics"))
          -- call Metacontract
          -- Input format: logdata ++ topics ++ [num_topics]
          -- Output format: SUCCESS([])
          ,(assert (callMc (Var "in_offset") (Add (Var "in_size") (Mul (Add (Var "num_topics") (Lit 1)) (Lit 0x20))) (Lit 0x00) (Lit 0x00)))
          ,(assert (Iszero Returndatasize))
          -- store event type in trace
          ,(Mstore (Var "record_ptr") (Var "num_topics"))
          ,(Assign "record_ptr" (Add (Var "record_ptr") (Lit 0x20)))
          -- store sha3(logdata ++ topics) in trace
          ,(Mstore (Var "record_ptr") (Sha3 (Var "in_offset")
                                            (Add (Var "in_size") (Mul (Var "num_topics") (Lit 0x20)))))
          ,(Assign "record_ptr" (Add (Var "record_ptr") (Lit 0x20)))
          -- update trace pointer
          ,(setTracePtr (Var "record_ptr"))
          -- restore backup
          ,(restore (Add (Var "in_offset") (Var "in_size"))
                    (Mul (Add (Var "num_topics") (Lit 1)) (Lit 0x20)))])

procCall = let regularCall = Scope
                 [(Let "in_end" (Add (Var "in_offset") (Var "in_size")))
                 -- compute sha3(sha3(input) ++ [to, value]) for trace
                 ,(Mstore (Lit $ backupMOffset + 0x00) (Sha3 (Var "in_offset") (Var "in_size")))
                 ,(Mstore (Lit $ backupMOffset + 0x20) (Var "to"))
                 ,(Mstore (Lit $ backupMOffset + 0x40) (Var "value"))
                 ,(Let "tracehash" (Sha3 (Lit backupMOffset) (Lit 0x60)))
                 -- backup three words following input
                 ,(backup (Var "in_end") (Lit 0x60))
                 -- append [to, value, 5] to input
                 ,(Mstore (Add (Var "in_end") (Lit 0x00)) (Var "to"))
                 ,(Mstore (Add (Var "in_end") (Lit 0x20)) (Var "value"))
                 ,(Mstore (Add (Var "in_end") (Lit 0x40)) (Lit 5) {- 5 is code for CALL-})
                 -- Call MC
                 -- Input format: input ++ [to, value, 5]
                 -- Output format: [success] ++ output
                 ,(assert (callMc (Var "in_offset") (Add (Var "in_size") (Lit 0x60)) (Lit 0x00) (Lit 0x00)))
                 ,(assert (M.leq (Lit 0x20) Returndatasize))
                 -- store event type in trace
                 ,(Mstore (Var "record_ptr") (Lit 5))
                 ,(Assign "record_ptr" (Add (Var "record_ptr") (Lit 0x20)))
                 -- store hash in trace
                 ,(Mstore (Var "record_ptr") (Var "tracehash"))
                 ,(Assign "record_ptr" (Add (Var "record_ptr") (Lit 0x20)))
                 -- store success in trace and assign return value
                 ,(Returndatacopy (Var "record_ptr") (Lit 0x00) (Lit 0x20))
                 ,(Assign "success" (Mload (Var "record_ptr")))
                 ,(Assign "record_ptr" (Add (Var "record_ptr") (Lit 0x20)))
                 -- store size in trace
                 ,(Mstore (Var "record_ptr") (Sub Returndatasize (Lit 0x20)))
                 ,(Assign "record_ptr" (Add (Var "record_ptr") (Lit 0x20)))
                 -- store output in trace
                 ,(Returndatacopy (Var "record_ptr") (Lit 0x20) (Sub Returndatasize (Lit 0x20)))
                 ,(Assign "record_ptr" (Add (Var "record_ptr") (Sub Returndatasize (Lit 0x20))))
                 -- update trace length
                 ,(setTracePtr (Var "record_ptr"))
                 -- restore backup
                 ,(restore (Var "in_end") (Lit 0x60))
                 -- output result
                 ,(Returndatacopy (Var "out_offset") (Lit 0x20) (min_ (Var "out_size") (Sub Returndatasize (Lit 0x20))))
                 ] in
           Proc "call" ["gas", "to", "value", "in_offset", "in_size", "out_offset", "out_size"] "success" (Scope
           [(Assign "in_offset" (Add (Var "in_offset") (Lit memoryMOffset)))
           ,(Assign "out_offset" (Add (Var "out_offset") (Lit memoryMOffset)))
           ,(IfElse (And (Lt (Lit 0) (Var "to")) (M.leq (Var "to") (Lit maxPrecompileAddress)))
                -- TODO(lorenzb): check behaviour of precompiles when called with non-zero value
                (Scope [(assert (Iszero (Var "value")))
                       ,(Assign "success" (Call (Var "gas") (Var "to") (Var "value") (Var "in_offset") (Var "in_size") (Var "out_offset") (Var "out_size")))])
                (Scope [(Let "record_ptr" getTracePtr)
                       ,(IfElse (Eq (Var "to") (ProcCall "mc" []))
                             (Scope [(IfElse (Gt (Var "value") (Balance (ProcCall "mc" [])))
                                          (Scope [(Assign "success" (Lit 0))])
                                          (Scope [(backup (Sub (Var "in_offset") (Lit 0x60)) (Lit 0x60))
                                                 ,(Mstore (Sub (Var "in_offset") (Lit 0x60)) (ProcCall "mc" []))
                                                 ,(Mstore (Sub (Var "in_offset") (Lit 0x40)) (Var "value"))
                                                 ,(Mstore (Sub (Var "in_offset") (Lit 0x20)) (Var "in_size"))
                                                 ,(Assign "success" (callHead Gas Address (Lit 0) (Sub (Var "in_offset") (Lit 0x60)) (Add (Var "in_size") (Lit 0x60)) (Lit 0x00) (Lit 0x00)))
                                                 -- Restore backup
                                                 ,(restore (Sub (Var "in_offset") (Lit 0x60)) (Lit 0x60))
                                                 -- Append trace
                                                 ,(Let "call_trace_size" (returndataload (Lit 0x20)))
                                                 ,(Returndatacopy (Var "record_ptr") (Lit 0x40) (Var "call_trace_size"))
                                                 ,(Assign "record_ptr" (Add (Var "record_ptr") (Var "call_trace_size")))
                                                 -- update trace length
                                                 ,(setTracePtr (Var "record_ptr"))
                                                 -- returnvalue
                                                 ,(Let "returndata_start" (Add (Var "call_trace_size") (Lit 0x40)))
                                                 ,(Let "returndata_size" (Sub Returndatasize (Var "returndata_start")))
                                                 ,(Returndatacopy (Var "out_offset") (Var "returndata_start") (min_ (Var "out_size") (Var "returndata_size")))]))])
                             regularCall)]))])

procBalance = Proc "balance" ["address"] "balance" (Scope
              [(Let "record_ptr" getTracePtr)
              -- get balance
              ,(Assign "balance" (Balance (Var "address")))
              -- store event type in trace
              ,(Mstore (Var "record_ptr") (Lit 6))
              ,(Assign "record_ptr" (Add (Var "record_ptr") (Lit 0x20)))
              -- store address in trace
              ,(Mstore (Var "record_ptr") (Var "address"))
              ,(Assign "record_ptr" (Add (Var "record_ptr") (Lit 0x20)))
              -- store balance in trace
              ,(Mstore (Var "record_ptr") (Var "balance"))
              ,(Assign "record_ptr" (Add (Var "record_ptr") (Lit 0x20)))
              -- update trace length
              ,(setTracePtr (Var "record_ptr"))])

-- Output format:
-- [1, trace_size] ++ trace ++ returndata
procDone = Proc "done" ["success", "offset", "size"] "_" (Scope
           [(Assign "offset" (Add (Var "offset") (Lit memoryMOffset)))
           ,(Let "trace_size" traceSize)
           ,(memcpyPrecomp (Add (Lit traceMOffset) (Var "trace_size"))
                           (Var "offset")
                           (Var "size"))
           ,(Mstore (Lit $ traceMOffset - 0x40) (Lit 1))
           ,(Mstore (Lit $ traceMOffset - 0x20) (Var "trace_size"))
           ,(IfElse (Var "success")
                 (Scope [(Return (Lit $ traceMOffset - 0x40) (M.add3 (Lit 0x40) (Var "trace_size") (Var "size")))])
                 (Scope [(Revert (Lit $ traceMOffset - 0x40) (M.add3 (Lit 0x40) (Var "trace_size") (Var "size")))]))])
