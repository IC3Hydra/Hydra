module EVM.Instrumentation.HeadOne
where

import           Data.List
import qualified EVM.Address as A
import           EVM.Bytecode
import           EVM.BytecodePlus
import           EVM.Instrumentation.Common
import           EVM.While
import qualified EVM.While.Macros as M
import           Prelude          hiding (EQ, GT, LT)

instrumentOps :: A.Address -> [OpcodePlus] -> [OpcodePlus]
instrumentOps mc = concatMap aux
    where aux (Op STOP)         = [ Push 0, Op (DUP 1), Push 1
                                  -- [success == 1, 0, 0]
                                  , ProcedureCall $ procTag "done"
                                  -- Control never reaches this point
                                  ]
          aux (Op ADDRESS)      = [ Push (A.toInteger mc)
                                  ]
          aux (Op BALANCE)      = underflowGuard 1 ++
                                  [ ProcedureCall $ procTag "balance"
                                  ]
          aux (Op CALLER)       = [ Push 0x00
                                  -- [0x00]
                                  , Op CALLDATALOAD
                                  -- [caller]
                                  ]
          aux (Op CALLVALUE)    = [ Push 0x20
                                  -- [0x20]
                                  , Op CALLDATALOAD
                                  -- [callvalue]
                                  ]
          aux (Op CALLDATALOAD) = underflowGuard 1 ++
                                  [ ProcedureCall $ procTag "calldataload"
                                  ]
          aux (Op CALLDATASIZE) = [ Push 0x60
                                  -- [0x60]
                                  , Op CALLDATASIZE
                                  -- [calldatasize, 0x60]
                                  , Op SUB
                                  -- [calldatasize - 0x60]
                                  ]
          aux (Op CALLDATACOPY) = underflowGuard 3 ++
                                  [ ProcedureCall $ procTag "calldatacopy"
                                  , Op POP
                                  ]
          aux (Op EXTCODESIZE)  = underflowGuard 1 ++
                                  [ ProcedureCall $ procTag "extcodesize"
                                  ]
          -- TODO(lorenzb): All ops that offset memory are vulnerable to overflow (MLOAD, MSTORE, MSTORE8, CALLDATALOAD, ...)
          aux (Op MLOAD)        = underflowGuard 1 ++
                                  [ ProcedureCall $ procTag "offsetMem"
                                  -- [mem_start + offset]
                                  , Op MLOAD
                                  -- [M[mem_start + offset]]
                                  ]
          aux (Op MSTORE)       = underflowGuard 2 ++
                                  [ ProcedureCall $ procTag "offsetMem"
                                  -- [mem_start + offset, word]
                                  , Op MSTORE
                                  -- []
                                  ]
          aux (Op MSTORE8)      = underflowGuard 2 ++
                                  [ ProcedureCall $ procTag "offsetMem"
                                  -- [mem_start + offset, byte]
                                  , Op MSTORE8
                                  -- []
                                  ]
          aux (Op SHA3)         = underflowGuard 2 ++
                                  [ ProcedureCall $ procTag "sha3"
                                  ]
          aux (Op JUMP)         = [ TagJump "jumptable"
                                  ]
          aux (Op JUMPI)        = [ Op (SWAP 1)
                                  -- [should_jump, jumpdest]
                                  , TagJumpi "jumptable" -- ⤳ S [V "pc"]
                                  -- [jumpdest]
                                  , Op POP
                                  -- []
                                  ]
          aux (Op MSIZE)        = [ Push memoryMOffset
                                  -- [mem_start]
                                  , Op MSIZE
                                  -- [msize, mem_start]
                                  , Op SUB
                                  -- [msize - mem_start]
                                  ]
          aux (Op LOG0)         = underflowGuard 2 ++
                                  [ ProcedureCall $ procTag "log0"
                                  , Op POP
                                  ]
          aux (Op LOG1)         = underflowGuard 3 ++
                                  [ ProcedureCall $ procTag "log1"
                                  , Op POP
                                  ]
          aux (Op LOG2)         = underflowGuard 4 ++
                                  [ ProcedureCall $ procTag "log2"
                                  , Op POP
                                  ]
          aux (Op LOG3)         = underflowGuard 5 ++
                                  [ ProcedureCall $ procTag "log3"
                                  , Op POP
                                  ]
          aux (Op LOG4)         = underflowGuard 6 ++
                                  [ ProcedureCall $ procTag "log4"
                                  , Op POP
                                  ]
          aux (Op CALL)         = underflowGuard 7 ++
                                  [ ProcedureCall $ procTag "call"
                                  -- [success]
                                  ]
          aux (Op RETURN)       = underflowGuard 2 ++
                                  [ Push 1
                                  -- [success == 1, offset, size]
                                  , ProcedureCall $ procTag "done"
                                  -- Control never reaches this point
                                  ]
          aux (Op REVERT)       = underflowGuard 2 ++
                                  [ Push 0
                                  -- [success == 0, offset, size]
                                  , ProcedureCall $ procTag "done"
                                  -- Control never reaches this point
                                  ]
          aux (Op (Unknown _))  = [ Push 0, Op (DUP 1), Op (DUP 1)
                                  -- [success == 0, offset == 0, size = 0]
                                  , ProcedureCall $ procTag "done"
                                  -- Control never reaches this point
                                  ]
          aux op                = [ op ]


-- TODO(lorenzb): fix order
procs :: A.Address -> [Proc]
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
           , procCalldataload
           , procCalldatacopy
           , procExtcodesize
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
                            ,(checkOrDie (M.leq traceSize (Lit traceMSize)))])

traceSize = (Sub getTracePtr (Lit traceMOffset))

backup e1 e2 = (memcpyNoalias (Lit backupMOffset) e1 e2)

restore e1 e2 = (memcpyNoalias e1 (Lit backupMOffset) e2)

callMc e1 e2 e3 e4 = (Call Gas (ProcCall "mc" []) (Lit 0) e1 e2 e3 e4)

procInit = Proc "init" [] "_" (Scope
           [(checkOrDie (M.and3 (Or (Eq Caller (ProcCall "mc" [])) (Eq Caller Address))
                                (Iszero Callvalue)
                                (Gt Calldatasize (Lit (0x40-1)))))
           ,(checkOrErr errorIncorrectCalldataSize (Eq (Sub Calldatasize (Lit 0x60)) (Calldataload (Lit 0x40))))
           ,(Mstore (Lit (memoryMOffset - 0x20)) (Lit 1))
           ,(setTracePtr (Lit traceMOffset))])

procCalldataload = Proc "calldataload" ["offset"] "data" (Scope
                   [(Assign "data" (Calldataload (Add (min_ (Var "offset") (Lit maxMem)) (Lit 0x60))))])

procCalldatacopy = Proc "calldatacopy" ["dst", "src", "size"] "_" (Scope
                   [(M.if_ (Var "size")
                         (Scope [(Calldatacopy (offsetMem (Var "dst"))
                                               (Add (min_ (Var "src") (Lit maxMem)) (Lit 0x60))
                                               (Var "size"))]))])

procExtcodesize = Proc "extcodesize" ["address"] "size" (Scope
                  [(Let "record_ptr" getTracePtr)
                  -- get balance
                  ,(Assign "size" (Extcodesize (Var "address")))
                  -- store event type in trace
                  ,(Mstore (Var "record_ptr") (Lit 7))
                  ,(M.inc "record_ptr" (Lit 0x20))
                  -- store address in trace
                  ,(Mstore (Var "record_ptr") (Var "address"))
                  ,(M.inc "record_ptr" (Lit 0x20))
                  -- store size in trace
                  ,(Mstore (Var "record_ptr") (Var "size"))
                  ,(M.inc "record_ptr" (Lit 0x20))
                  -- update trace length
                  ,(setTracePtr (Var "record_ptr"))])

procLog = Proc "log" ["num_topics", "in_offset", "in_size", "topic1", "topic2", "topic3", "topic4"] "_" (Scope
          [(IfElse (Var "in_size")
                (Scope [(Assign "in_offset" (offsetMem (Var "in_offset")))])
                (Scope [(Assign "in_offset" (Lit memoryMOffset))]))
          ,(Let "record_ptr" getTracePtr)
          ,(Let "prefix_size" (Add (Mul (Var "num_topics") (Lit 0x20)) (Lit 0x20)))
          ,(Let "prefix_offset" (Sub (Var "in_offset") (Var "prefix_size")))
          -- back up words preceding input
          ,(backup (Var "prefix_offset") (Var "prefix_size"))
          -- prepend to input: [num_topics, topic1 .. topicn]
          ,(Mstore (Var "prefix_offset") (Var "num_topics"))
          ,(M.if_ (Lt (Lit 0) (Var "num_topics")) (Scope [(Mstore (Add (Var "prefix_offset") (Lit 0x20)) (Var "topic1"))]))
          ,(M.if_ (Lt (Lit 1) (Var "num_topics")) (Scope [(Mstore (Add (Var "prefix_offset") (Lit 0x40)) (Var "topic2"))]))
          ,(M.if_ (Lt (Lit 2) (Var "num_topics")) (Scope [(Mstore (Add (Var "prefix_offset") (Lit 0x60)) (Var "topic3"))]))
          ,(M.if_ (Lt (Lit 3) (Var "num_topics")) (Scope [(Mstore (Add (Var "prefix_offset") (Lit 0x80)) (Var "topic4"))]))
          -- call Metacontract
          -- Input format: [num_topics] ++ topics ++ logdata
          -- Output format: SUCCESS([])
          ,(checkOrDie (callMc (Var "prefix_offset") (Add (Var "prefix_size") (Var "in_size")) (Lit 0x00) (Lit 0x00)))
          ,(checkOrDie (Iszero Returndatasize))
          -- store event type in trace
          ,(Mstore (Var "record_ptr") (Var "num_topics"))
          ,(Assign "record_ptr" (Add (Var "record_ptr") (Lit 0x20)))
          -- store sha3(sha3(logdata) ++ topics) in trace
          ,(Mstore (Var "prefix_offset") (Sha3 (Var "in_offset") (Var "in_size")))
          ,(Mstore (Var "record_ptr") (Sha3 (Var "prefix_offset") (Var "prefix_size")))
          ,(Assign "record_ptr" (Add (Var "record_ptr") (Lit 0x20)))
          -- update trace pointer
          ,(setTracePtr (Var "record_ptr"))
          -- restore backup
          ,(restore (Var "prefix_offset") (Var "prefix_size"))])

procCall = let regularCall = Scope
                 [(Let "in_end" (Add (Var "in_offset") (Var "in_size")))
                 -- compute sha3(sha3(input) ++ [to, value]) for trace
                 ,(Mstore (Lit $ backupMOffset + 0x00) (Sha3 (Var "in_offset") (Var "in_size")))
                 ,(Mstore (Lit $ backupMOffset + 0x20) (Var "to"))
                 ,(Mstore (Lit $ backupMOffset + 0x40) (Var "value"))
                 ,(Let "tracehash" (Sha3 (Lit backupMOffset) (Lit 0x60)))
                 -- backup three words preceding input
                 ,(backup (Sub (Var "in_offset") (Lit 0x60)) (Lit 0x60))
                 -- prepend [5, to, value] to input
                 ,(Mstore (Sub (Var "in_offset") (Lit 0x60)) (Lit 5))
                 ,(Mstore (Sub (Var "in_offset") (Lit 0x40)) (Var "to"))
                 ,(Mstore (Sub (Var "in_offset") (Lit 0x20)) (Var "value"))
                 -- Call MC
                 -- Input format: [5, to, value] ++ input
                 -- Output format: [success] ++ output
                 ,(checkOrDie (callMc (Sub (Var "in_offset") (Lit 0x60)) (Add (Var "in_size") (Lit 0x60)) (Lit 0x00) (Lit 0x00)))
                 ,(checkOrDie (M.leq (Lit 0x20) Returndatasize))
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
                 ,(restore (Sub (Var "in_offset") (Lit 0x60)) (Lit 0x60))
                 -- output result
                 ,(Returndatacopy (Var "out_offset") (Lit 0x20) (min_ (Var "out_size") (Sub Returndatasize (Lit 0x20))))
                 ] in
           Proc "call" ["gas", "to", "value", "in_offset", "in_size", "out_offset", "out_size"] "success" (Scope
           [(Assign "in_offset" (Add (Var "in_offset") (Lit memoryMOffset)))
           ,(Assign "out_offset" (Add (Var "out_offset") (Lit memoryMOffset)))
           ,(IfElse (And (Lt (Lit 0) (Var "to")) (M.leq (Var "to") (Lit maxPrecompileAddress)))
                -- TODO(lorenzb): check behaviour of precompiles when called with non-zero value
                (Scope [(checkOrDie (Iszero (Var "value")))
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
                             regularCall)]))
           -- touch last byte of input and output so that MSIZE stays correct even if call failed, etc...
           -- TODO(lorenzb): When implementing proper overflow checks, do this analogously to HeadN
           ,(Discard (Mload (Sub (Add (Var "in_offset") (Var "in_size")) (Lit 0x20))))
           ,(Discard (Mload (Sub (Add (Var "out_offset") (Var "out_size")) (Lit 0x20))))])

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
