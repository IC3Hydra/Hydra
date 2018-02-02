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
          aux (Op MLOAD)        = [ Push 0x00
                                  -- [0, offset]
                                  , Op $ MLOAD
                                  -- [mem_start, offset]
                                  , Op $ ADD
                                  -- [mem_start + offset]
                                  , Op $ MLOAD
                                  -- [M[mem_start + offset]]
                                  ]
          aux (Op MSTORE)       = [ Push 0x00
                                  -- [0, offset, word]
                                  , Op $ MLOAD
                                  -- [mem_start, offset, word]
                                  , Op $ ADD
                                  -- [mem_start + offset, word]
                                  , Op $ MSTORE
                                  -- []
                                  ]
          aux (Op MSTORE8)      = [ Push 0x00
                                  -- [0, offset, byte]
                                  , Op $ MLOAD
                                  -- [mem_start, offset, byte]
                                  , Op $ ADD
                                  -- [mem_start + offset, byte]
                                  , Op $ MSTORE8
                                  -- []
                                  ]
          aux (Op SHA3)         = [ Push 0x00
                                  -- [0, offset, size]
                                  , Op $ MLOAD
                                  -- [mem_start, offset, size]
                                  , Op $ ADD
                                  -- [mem_start + offset, size]
                                  , Op $ SHA3
                                  -- [hash]
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
          aux (Op MSIZE)        = [ Push 0x00
                                  -- [0]
                                  , Op $ MLOAD
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
           ]

memoryStashSize = 0x20 * 200

backupOffset = 0x20

maxBackupSize = traceOffset - backupOffset

traceOffset = 0x20 * 6

maxTraceSize = memoryStashSize - traceOffset

assert e = M.if_ (Iszero e) (Scope [Revert (Lit 0x00) (Lit 0x00)])
callMc e1 e2 e3 e4 = (Call Gas (ProcCall "mc" []) (Lit 0) e1 e2 e3 e4)

procInit = Proc "init" [] "_" (Scope
           [(checkOrDie (M.and3 (Or (Eq Caller (ProcCall "mc" [])) (Eq Caller Address))
                                (Iszero Callvalue)
                                (Gt Calldatasize (Lit (0x40-1)))))
           ,(Mstore (Lit 0x00) (Lit memoryStashSize))
           ,(Mstore (Lit (memoryStashSize - 0x20)) (Lit 1))])

procCalldatacopy = Proc "calldatacopy" ["dst", "src", "size"] "_" (Scope
                   [(Discard (Lit 314159265358979)),(Calldatacopy (Add (Var "dst") (Mload (Lit 0x00))) (Add (Var "src") (Lit 0x40)) (Var "size"))])

procLog0 = Proc "log0" ["in_offset", "in_size"] "_" (Scope
           [Discard (ProcCall "log" [(Lit 0), (Var "in_offset"), (Var "in_size"), (Lit 0), (Lit 0), (Lit 0), (Lit 0)])])

procLog1 = Proc "log1" ["in_offset", "in_size", "topic1"] "_" (Scope
           [Discard (ProcCall "log" [(Lit 1), (Var "in_offset"), (Var "in_size"), (Var "topic1"), (Lit 0), (Lit 0), (Lit 0)])])

procLog2 = Proc "log2" ["in_offset", "in_size", "topic1", "topic2"] "_" (Scope
           [Discard (ProcCall "log" [(Lit 2), (Var "in_offset"), (Var "in_size"), (Var "topic1"), (Var "topic2"), (Lit 0), (Lit 0)])])

procLog3 = Proc "log3" ["in_offset", "in_size", "topic1", "topic2", "topic3"] "_" (Scope
           [Discard (ProcCall "log" [(Lit 3), (Var "in_offset"), (Var "in_size"), (Var "topic1"), (Var "topic2"), (Var "topic3"), (Lit 0)])])

procLog4 = Proc "log4" ["in_offset", "in_size", "topic1", "topic2", "topic3", "topic4"] "_" (Scope
           [Discard (ProcCall "log" [(Lit 4), (Var "in_offset"), (Var "in_size"), (Var "topic1"), (Var "topic2"), (Var "topic3"), (Var "topic4")])])

procLog = Proc "log" ["num_topics", "in_offset", "in_size", "topic1", "topic2", "topic3", "topic4"] "_" (Scope
          [(Assign "in_offset" (Add (Var "in_offset") (Mload (Lit 0x00))))
          ,(Let "trace_size" (Mload (Lit traceOffset)))
          ,(Let "record_start" (Add (Var "trace_size") (Lit (traceOffset + 0x20))))
          ,(Let "record_ptr" (Var "record_start"))
          -- back up words following input
          ,(memcpyNoalias (Lit backupOffset)
                          (Add (Var "in_offset") (Var "in_size"))
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
          -- update trace length
          ,(Assign "trace_size" (Add (Var "trace_size") (Sub (Var "record_ptr") (Var "record_start"))))
          ,(assert (M.leq (Var "trace_size") (Lit maxTraceSize)))
          ,(Mstore (Lit traceOffset) (Var "trace_size"))
          -- restore backup
          ,(memcpyNoalias (Add (Var "in_offset") (Var "in_size"))
                          (Lit backupOffset)
                          (Mul (Add (Var "num_topics") (Lit 1)) (Lit 0x20)))])

procCall = let regularCall = Scope
                 [(Let "in_end" (Add (Var "in_offset") (Var "in_size")))
                 -- compute sha3(sha3(input) ++ [to, value]) for trace
                 ,(Mstore (Lit $ backupOffset + 0x00) (Sha3 (Var "in_offset") (Var "in_size")))
                 ,(Mstore (Lit $ backupOffset + 0x20) (Var "to"))
                 ,(Mstore (Lit $ backupOffset + 0x40) (Var "value"))
                 ,(Let "tracehash" (Sha3 (Lit backupOffset) (Lit 0x60)))
                 -- backup three words following input
                 ,(memcpyNoalias (Lit backupOffset)
                                 (Var "in_end")
                                 (Lit 0x60))
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
                 ,(Assign "trace_size" (Add (Var "trace_size") (Sub (Var "record_ptr") (Var "record_start"))))
                 ,(assert (M.leq (Var "trace_size") (Lit maxTraceSize)))
                 ,(Mstore (Lit traceOffset) (Var "trace_size"))
                 -- restore backup
                 ,(memcpyNoalias (Var "in_end")
                                 (Lit backupOffset)
                                 (Lit 0x60))
                 -- output result
                 ,(Returndatacopy (Var "out_offset") (Lit 0x20) (min_ (Var "out_size") (Sub Returndatasize (Lit 0x20))))
                 ] in
           Proc "call" ["gas", "to", "value", "in_offset", "in_size", "out_offset", "out_size"] "success" (Scope
           [(Assign "in_offset" (Add (Var "in_offset") (Mload (Lit 0x00))))
           ,(Assign "out_offset" (Add (Var "out_offset") (Mload (Lit 0x00))))
           ,(IfElse (And (Lt (Lit 0) (Var "to")) (M.leq (Var "to") (Lit maxPrecompileAddress)))
                -- TODO(lorenzb): check behaviour of precompiles when called with non-zero value
                (Scope [(assert (Iszero (Var "value")))
                       ,(Assign "success" (Call (Var "gas") (Var "to") (Var "value") (Var "in_offset") (Var "in_size") (Var "out_offset") (Var "out_size")))])
                (Scope [(Let "trace_size" (Mload (Lit traceOffset)))
                       ,(Let "record_start" (Add (Var "trace_size") (Lit (traceOffset + 0x20))))
                       ,(Let "record_ptr" (Var "record_start"))
                       ,(IfElse (Eq (Var "to") (ProcCall "mc" []))
                             (Scope [(IfElse (Gt (Var "value") (Balance (ProcCall "mc" [])))
                                          (Scope [(Assign "success" (Lit 0))])
                                          (Scope [(memcpyNoalias (Lit backupOffset) (Sub (Var "in_offset") (Lit 0x60)) (Lit 0x60))
                                                 ,(Mstore (Sub (Var "in_offset") (Lit 0x60)) (ProcCall "mc" []))
                                                 ,(Mstore (Sub (Var "in_offset") (Lit 0x40)) (Var "value"))
                                                 ,(Mstore (Sub (Var "in_offset") (Lit 0x20)) (Var "in_size"))
                                                 ,(Assign "success" (callHead Gas Address (Lit 0) (Sub (Var "in_offset") (Lit 0x60)) (Add (Var "in_size") (Lit 0x60)) (Lit 0x00) (Lit 0x00)))
                                                 -- Restore backup
                                                 ,(memcpyNoalias (Sub (Var "in_offset") (Lit 0x60)) (Lit backupOffset) (Lit 0x60))
                                                 -- Append trace
                                                 ,(Let "call_trace_size" (returndataload (Lit 0x20)))
                                                 ,(Returndatacopy (Var "record_ptr") (Lit 0x40) (Var "call_trace_size"))
                                                 ,(Assign "record_ptr" (Add (Var "record_ptr") (Var "call_trace_size")))
                                                 -- update trace length
                                                 ,(Assign "trace_size" (Add (Var "trace_size") (Sub (Var "record_ptr") (Var "record_start"))))
                                                 ,(assert (M.leq (Var "trace_size") (Lit maxTraceSize)))
                                                 ,(Mstore (Lit traceOffset) (Var "trace_size"))
                                                 -- returnvalue
                                                 ,(Let "returndata_start" (Add (Var "call_trace_size") (Lit 0x40)))
                                                 ,(Let "returndata_size" (Sub Returndatasize (Var "returndata_start")))
                                                 ,(Returndatacopy (Var "out_offset") (Var "returndata_start") (min_ (Var "out_size") (Var "returndata_size")))]))])
                             regularCall)]))])

procBalance = Proc "balance" ["address"] "balance" (Scope
              [(Let "trace_size" (Mload (Lit traceOffset)))
              ,(Let "record_start" (Add (Var "trace_size") (Lit (traceOffset + 0x20))))
              ,(Let "record_ptr" (Var "record_start"))
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
              ,(Assign "trace_size" (Add (Var "trace_size") (Sub (Var "record_ptr") (Var "record_start"))))
              ,(assert (M.leq (Var "trace_size") (Lit maxTraceSize)))
              ,(Mstore (Lit traceOffset) (Var "trace_size"))])

-- Output format:
-- [1, trace_size] ++ trace ++ returndata
procDone = Proc "done" ["success", "offset", "size"] "_" (Scope
           [(Assign "offset" (Add (Var "offset") (Mload (Lit 0x00))))
           --,(Assign "size" (Add (Var "size") (Mload (Lit 0x00))))
           ,(Let "trace_size_plus" (Add (Mload (Lit traceOffset)) (Lit 0x20)))
           ,(memcpyPrecomp (Add (Lit traceOffset) (Var "trace_size_plus"))
                           (Var "offset")
                           (Var "size"))
           ,(Mstore (Lit $ traceOffset - 0x20) (Lit 1))
           ,(IfElse (Var "success")
                 (Scope [(Return (Lit $ traceOffset - 0x20) (M.add3 (Lit 0x20) (Var "trace_size_plus") (Var "size")))])
                 (Scope [(Revert (Lit $ traceOffset - 0x20) (M.add3 (Lit 0x20) (Var "trace_size_plus") (Var "size")))]))])

procUnknownJumpdest = Proc "unknownJumpdest" [] "_" (Scope
                      [(Discard (Lit 314159265358979)), (Discard (ProcCall "done" [Lit 0, Lit 0, Lit 0]))])
