module EVM.Instrumentation.HeadN
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
          aux (Op CALLDATASIZE) = [ ProcedureCall $ procTag "calldatasize"
                                  ]
          aux (Op CALLDATACOPY) = underflowGuard 3 ++
                                  [ ProcedureCall $ procTag "calldatacopy"
                                  , Op POP
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
                                  , TagJumpi "jumptable"
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
procs mc = [ procMemcpyNoalias
           , procMin
           , procLog0
           , procLog1
           , procLog2
           , procLog3
           , procLog4
           , procLog
           , procCall
           , procBalance
           , procSha3
           , procDone
           , procCalldatacopy
           , procCalldataload
           , procCalldatasize
           , procInit
           , procMc mc
           , procUnknownJumpdest
           , procReturndataload
           , procCallHead
           , procOffsetMem memoryMOffset
           ]

-- TODO(lorenzb): Ensure that instrumentation never increases MSIZE

-- TODO(lorenzb): Unify constants between heads
tracePtrMOffset :: Integer
tracePtrMOffset = 0x00

specialMOffset :: Integer
specialMOffset = tracePtrMOffset + 0x20

memoryMOffset :: Integer
memoryMOffset = specialMOffset + 200 * 0x20

getTracePtr = (Mload (Lit tracePtrMOffset))

setTracePtr e = (Mstore (Lit tracePtrMOffset) e)

backup e1 e2 = (memcpyNoalias (Lit specialMOffset) e1 e2)

restore e1 e2 = (memcpyNoalias e1 (Lit specialMOffset) e2)

calldatasizeCOffset = 0x40
calldataCOffset = 0x60

traceStart = (M.add3 (Lit calldataCOffset) (Calldataload (Lit calldatasizeCOffset)) (Lit 0x20))
traceEnd = Calldatasize

procInit = Proc "init" [] "_" (Scope
           [(checkOrDie (M.and3 (Or (Eq Caller (ProcCall "mc" [])) (Eq Caller Address))
                                (Iszero Callvalue)
                                (Gt Calldatasize (Lit (0x40-1)))))
           -- TODO(lorenzb): Memory initialization?
           ,(setTracePtr traceStart)
           --,(Mstore (Lit 0x00) (Lit memoryStashSize))
           --,(Mstore (Lit (memoryStashSize - 0x20)) (Lit 1))
           ])


procBalance = Proc "balance" ["address"] "balance" (Scope
              [(Let "trace_ptr" getTracePtr)
              ,(checkOrErr disagreement (Eq (Lit 6) (Calldataload (Var "trace_ptr"))))
              ,(Assign "trace_ptr" (Add (Var "trace_ptr") (Lit 0x20)))
              ,(checkOrErr disagreement (Eq (Var "address") (Calldataload (Var "trace_ptr"))))
              ,(Assign "trace_ptr" (Add (Var "trace_ptr") (Lit 0x20)))
              ,(Assign "balance" (Calldataload (Var "trace_ptr")))
              ,(Assign "trace_ptr" (Add (Var "trace_ptr") (Lit 0x20)))
              ,(setTracePtr (Var "trace_ptr"))])

procCalldataload = Proc "calldataload" ["offset"] "data" (Scope
                   -- TODO(lorenzb): Correct, but inefficient!
                   [(Let "memloc" (offsetMem (Lit 0x00)))
                   ,(Let "backup" (Mload (Var "memloc")))
                   ,(Discard (ProcCall "calldatacopy" [(Lit 0x00), (Var "offset"), (Lit 0x20)]))
                   ,(Assign "data" (Mload (Var "memloc")))
                   ,(Mstore (Var "memloc") (Var "backup"))])

procCalldatasize = Proc "calldatasize" [] "size" (Scope
                   [(Assign "size" (Calldataload (Lit calldatasizeCOffset)))])

procCalldatacopy = Proc "calldatacopy" ["dst", "src", "size"] "data" (Scope
                   [(IfElse (Iszero (Var "size"))
                         (Scope [])
                         (Scope [(Let "calldata_size" (Calldataload (Lit calldatasizeCOffset)))
                                -- Overflow protected adjustment for "dst"
                                ,(Assign "dst" (offsetMem (Var "dst")))
                                -- Overflow protected adjustment for "src"
                                ,(Assign "src" (Add (ProcCall "min" [(Var "src"), (Var "calldata_size")])
                                                    (Lit calldataCOffset)))
                                -- Overflow protection for "size"
                                ,(checkOrDie (Lt (Var "size") (Lit maxMem)))
                                -- How much actual calldata is there to copy?
                                ,(Let "copy" (ProcCall "min" [(Sub (Add (Lit calldataCOffset) (Var "calldata_size"))
                                                                   (Var "src"))
                                                             ,(Var "size")]))
                                -- Copy actual calldata
                                ,(Calldatacopy (Var "dst")
                                               (Var "src")
                                               (Var "copy"))
                                -- Copy zeros
                                ,(Calldatacopy (Add (Var "dst") (Var "copy"))
                                               Calldatasize
                                               (Sub (Var "size") (Var "copy")))]))])

procLog = Proc "log" ["num_topics", "in_offset", "in_size", "topic1", "topic2", "topic3", "topic4"] "_" (Scope
          [(M.if_ (Var "in_size")
                (Scope [(Assign "in_offset" (offsetMem (Var "in_offset")))]))
          ,(Let "trace_ptr" getTracePtr)
          -- check log type vs trace
          ,(checkOrErr disagreement (Eq (Var "num_topics") (Calldataload (Var "trace_ptr"))))
          ,(Assign "trace_ptr" (Add (Var "trace_ptr") (Lit 0x20)))
          -- compute hash
          ,(Mstore (Lit $ specialMOffset + 0x00) (Sha3 (Var "in_offset") (Var "in_size")))
          ,(Mstore (Lit $ specialMOffset + 0x20) (Var "topic1"))
          ,(Mstore (Lit $ specialMOffset + 0x40) (Var "topic2"))
          ,(Mstore (Lit $ specialMOffset + 0x60) (Var "topic3"))
          ,(Mstore (Lit $ specialMOffset + 0x80) (Var "topic4"))
          ,(Let "hash" (Sha3 (Lit specialMOffset)
                             (Add (Mul (Var "num_topics") (Lit 0x20)) (Lit 0x20))))
          -- check trace
          ,(checkOrErr disagreement (Eq (Var "hash") (Calldataload (Var "trace_ptr"))))
          ,(Assign "trace_ptr" (Add (Var "trace_ptr") (Lit 0x20)))
          -- update trace ptr
          ,(setTracePtr (Var "trace_ptr"))])

-- TODO(lorenzb): Check size of input against maximum input size
procCall = let selfCall = (Scope
                   [(IfElse (Gt (Var "value") (Balance (ProcCall "mc" [])))
                         (Scope [(Assign "success" (Lit 0))])
                         (Scope [(Mstore (Lit $ specialMOffset + 0x00) (ProcCall "mc" []))
                                ,(Mstore (Lit $ specialMOffset + 0x20) (Var "value"))
                                ,(Mstore (Lit $ specialMOffset + 0x40) (Var "in_size"))
                                -- [caller, callvalue, calldata_size]
                                ,(memcpyNoalias (Lit $ specialMOffset + 0x60)
                                                (Var "in_offset")
                                                (Var "in_size"))
                                ,(Let "special_ptr" (Add (Lit $ specialMOffset + 0x60) (Var "in_size")))
                                -- [caller, callvalue, calldata_size] ++ calldata
                                ,(Let "remaining_trace_length" (Sub traceEnd getTracePtr))
                                ,(Mstore (Var "special_ptr") (Var "remaining_trace_length"))
                                ,(M.inc "special_ptr" (Lit 0x20))
                                -- [caller, callvalue, calldata_size] ++ calldata ++ [trace_size]
                                ,(Calldatacopy (Var "special_ptr")
                                               getTracePtr
                                               (Var "remaining_trace_length"))
                                ,(M.inc "special_ptr" (Var "remaining_trace_length"))
                                -- [caller, callvalue, calldata_size] ++ calldata ++ [trace_size]
                                -- Perform call
                                ,(Assign "success" (callHead Gas Address (Lit 0) (Lit specialMOffset) (Sub (Var "special_ptr") (Lit specialMOffset)) (Lit 0x00) (Lit 0x00)))
                                ,(Let "returndata_size" (Sub Returndatasize (Lit 0x40)))
                                -- must not return data from failed calls to self
                                ,(checkOrErr errorFailedSelfCallInteracted (Or (Var "success") (Iszero (Var "returndata_size"))))
                                ,(Let "trace_read" (returndataload (Lit 0x20)))
                                -- must not advance trace in failed calls to self
                                ,(checkOrErr errorFailedSelfCallInteracted (Or (Var "success") (Iszero (Var "trace_read"))))
                                -- advance trace
                                ,(setTracePtr (Add getTracePtr (Var "trace_read")))
                                ,(Returndatacopy (Var "out_offset")
                                                 (Lit 0x40)
                                                 (min_ (Var "returndata_size") (Var "out_size")))]))]) in
           let regularCall = (Scope
                   [(Let "trace_ptr" getTracePtr)
                   -- check call type vs trace
                   ,(checkOrErr disagreement (Eq (Lit 5) (Calldataload (Var "trace_ptr"))))
                   ,(Assign "trace_ptr" (Add (Var "trace_ptr") (Lit 0x20)))
                   -- check call input vs trace
                   ,(Mstore (Lit $ specialMOffset + 0x00) (Sha3 (Var "in_offset") (Var "in_size")))
                   ,(Mstore (Lit $ specialMOffset + 0x20) (Var "to"))
                   ,(Mstore (Lit $ specialMOffset + 0x40) (Var "value"))
                   ,(Let "inputhash" (Sha3 (Lit specialMOffset) (Lit 0x60)))
                   ,(checkOrErr disagreement (Eq (Var "inputhash") (Calldataload (Var "trace_ptr"))))
                   ,(Assign "trace_ptr" (Add (Var "trace_ptr") (Lit 0x20)))
                   -- get success from trace
                   ,(Assign "success" (Calldataload (Var "trace_ptr")))
                   ,(Assign "trace_ptr" (Add (Var "trace_ptr") (Lit 0x20)))
                   -- get returndata_size from trace
                   ,(Let "returndata_size" (Calldataload (Var "trace_ptr")))
                   ,(Assign "trace_ptr" (Add (Var "trace_ptr") (Lit 0x20)))
                   -- get returndata from trace
                   ,(Calldatacopy (Var "out_offset") (Var "trace_ptr") (min_ (Var "returndata_size") (Var "out_size")))
                   ,(Assign "trace_ptr" (Add (Var "trace_ptr") (Var "returndata_size")))
                   -- advance trace
                   ,(setTracePtr (Var "trace_ptr"))
                   ]) in
           Proc "call" ["gas", "to", "value", "in_offset", "in_size", "out_offset", "out_size"] "success" (Scope
           [(M.if_ (Var "in_size")
                 (Scope [(Assign "in_offset" (offsetMem (Var "in_offset")))
                        -- touch last byte of input so that MSIZE stays correct even if call failed, etc...
                        ,(Discard (Mload (Sub (Add (Var "in_offset") (Var "in_size")) (Lit 0x20))))]))
           ,(M.if_ (Var "out_size")
                 (Scope [(Assign "out_offset" (offsetMem (Var "out_offset")))
                        -- touch last byte of output so that MSIZE stays correct even if call failed, etc...
                        ,(Discard (Mload (Sub (Add (Var "out_offset") (Var "out_size")) (Lit 0x20))))]))
           ,(IfElse (And (Lt (Lit 0) (Var "to")) (M.leq (Var "to") (Lit maxPrecompileAddress)))
                -- TODO(lorenzb): check behaviour of precompiles when called with non-zero value
                (Scope [(checkOrErr errorPrecompileCalledWithNonzeroValue (Iszero (Var "value")))
                       -- TODO(lorenzb): Should we really forward the gas value here?
                       ,(Assign "success" (Call (Var "gas") (Var "to") (Var "value") (Var "in_offset") (Var "in_size") (Var "out_offset") (Var "out_size")))])
                (Scope [(IfElse (Eq (Var "to") (ProcCall "mc" []))
                             selfCall
                             regularCall)]))])

-- Output format:
-- [1, trace_read] ++ returndata
procDone = Proc "done" ["success", "offset", "size"] "_" (Scope
           [(Let "offset2" (Lit 0x40))
           ,(M.if_ (Var "size")
                 (Scope [(Assign "offset2" (offsetMem (Var "offset")))]))
           ,(Let "trace_read" (Sub getTracePtr traceStart))
           ,(M.dec "offset2" (Lit 0x40))
           ,(M.inc "size" (Lit 0x40))
           ,(Mstore (Var "offset2") (Lit 1))
           ,(Mstore (Add (Var "offset2") (Lit 0x20)) (Var "trace_read"))
           ,(IfElse (Var "success")
                 (Scope [(Return (Var "offset2") (Var "size"))])
                 (Scope [(Revert (Var "offset2") (Var "size"))]))])
