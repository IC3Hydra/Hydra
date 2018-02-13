module EVM.Instrumentation.Metacontract
( metacontract
) where

import qualified EVM.Address as A
import EVM.Bytecode (Opcode)
import EVM.Instrumentation.Common
import EVM.While
import qualified EVM.While.Macros as M
import Util

metacontract :: [A.Address] -> [Opcode]
metacontract heads = fromRight $ compileAndLower procs mc
    where procs = [ procHeadAddress heads
                  , procHeadCount heads
                  , procCallback
                  , procInner
                  , procPayBounty
                  , procOuter
                  , procReturndataload
                  , procCallHead
                  ]

-- TODO(lorenzb): Implement support for HYDRA_INIT!
-- TODO(lorenzb): Implement ABI checker

slocOuterState = 0
outerStateBountyPaid = 1
outerStateMutexOn = 2
outerStateMutexOff = 3

headAddress e = (ProcCall "headAddress" [e])
procHeadAddress heads = Proc "headAddress" ["index"] "address" (Scope $
                        [(checkOrErr errorHeadIndexTooLarge (Lt (Var "index") (Lit (fromIntegral $ length heads))))
                        ] ++ zipWith (\i a -> (M.if_ (Eq (Var "index") (Lit i)) (Scope [Assign "address" (Lit (A.toInteger a))]))) [0..] heads)

headCount = (ProcCall "headCount" [])
procHeadCount heads = Proc "headCount" [] "count" (Scope [(Assign "count" (Lit (fromIntegral $ length heads)))])

procCallback = Proc "callback" [] "_" (Scope
               [(checkOrErr errorInputTooSmall (M.leq (Lit 0x20) Calldatasize))
               ,(Let "type" (Calldataload (Lit 0x00)))
               ,(IfElse (M.leq (Var "type") (Lit 4))
                     -- LOG
                     (Scope [(Let "topic1" (Lit 0))
                            ,(Let "topic2" (Lit 0))
                            ,(Let "topic3" (Lit 0))
                            ,(Let "topic4" (Lit 0))
                            ,(M.if_ (M.leq (Lit 1) (Var "type"))
                                  (Scope [(Assign "topic1" (Calldataload (Lit 0x20)))]))
                            ,(M.if_ (M.leq (Lit 2) (Var "type"))
                                  (Scope [(Assign "topic2" (Calldataload (Lit 0x40)))]))
                            ,(M.if_ (M.leq (Lit 3) (Var "type"))
                                  (Scope [(Assign "topic3" (Calldataload (Lit 0x60)))]))
                            ,(M.if_ (M.leq (Lit 4) (Var "type"))
                                  (Scope [(Assign "topic4" (Calldataload (Lit 0x80)))]))
                            ,(Let "logdata_offset" (Add (Mul (Var "type") (Lit 0x20)) (Lit 0x20)))
                            ,(Let "logdata_size" (Sub Calldatasize (Var "logdata_offset")))
                            ,(Calldatacopy (Lit 0x00) (Var "logdata_offset") (Var "logdata_size"))
                            ,(M.if_ (Eq (Var "type") (Lit 0)) (Scope [(Log0 (Lit 0x00) (Var "logdata_size"))]))
                            ,(M.if_ (Eq (Var "type") (Lit 1)) (Scope [(Log1 (Lit 0x00) (Var "logdata_size") (Var "topic1"))]))
                            ,(M.if_ (Eq (Var "type") (Lit 2)) (Scope [(Log2 (Lit 0x00) (Var "logdata_size") (Var "topic1") (Var "topic2"))]))
                            ,(M.if_ (Eq (Var "type") (Lit 3)) (Scope [(Log3 (Lit 0x00) (Var "logdata_size") (Var "topic1") (Var "topic2") (Var "topic3"))]))
                            ,(M.if_ (Eq (Var "type") (Lit 4)) (Scope [(Log4 (Lit 0x00) (Var "logdata_size") (Var "topic1") (Var "topic2") (Var "topic3") (Var "topic4"))]))])
                     -- CALL
                     (Scope [(checkOrErr errorWrongInputFormat (Eq (Var "type") (Lit 5)))
                            ,(Let "to" (Calldataload (Lit 0x20)))
                            ,(Let "value" (Calldataload (Lit 0x40)))
                            ,(Let "datasize" (Sub Calldatasize (Lit 0x60)))
                            ,(Calldatacopy (Lit 0x00) (Lit 0x60) (Var "datasize"))
                            ,(Let "success" (Call Gas (Var "to") (Var "value") (Lit 0x00) (Var "datasize") (Lit 0x00) (Lit 0x00)))
                            ,(Mstore (Lit 0x00) (Var "success"))
                            ,(Returndatacopy (Lit 0x20) (Lit 0x00) Returndatasize)
                            ,(Return (Lit 0x00) (Add Returndatasize (Lit 0x20)))]))])


-- Input format: [caller, callvalue] ++ calldata
-- Output format:
--   Agreement, Success => SUCCESS([0x01] ++ returndata)
--   Agreement, Fail    => SUCCESS([0x00] ++ returndata)
--   Disagreement       => FAIL([0xd15a9])
--   Internal error     => Some other FAIL
procInner = Proc "inner" [] "_" (Scope
            [(checkOrErr errorInputTooSmall (M.leq (Lit 0x40) Calldatasize))
            -- Call first head to construct trace
            -- Input format: `[caller, callvalue, calldata_size] ++ calldata`
            -- Output format:
            -- - Success: `SUCCESS([1, trace_size] ++ trace ++ returndata)`
            -- - Failure: `FAIL([1, trace_size] ++ trace ++ returndata)`
            -- - OOG: `FAIL([])`
            -- - Instrumentation Error: `FAIL([code])`
            ,(Let "caller" (Calldataload (Lit 0x00)))
            ,(Let "callvalue" (Calldataload (Lit 0x20)))
            -- Mem: []
            ,(Mstore (Lit 0x00) (Var "caller"))
            -- Mem: [caller]
            ,(Mstore (Lit 0x20) (Var "callvalue"))
            -- Mem: [caller, callvalue]
            ,(Mstore (Lit 0x40) (Sub Calldatasize (Lit 0x40)))
            -- Mem: [caller, callvalue, calldatasize]
            ,(Calldatacopy (Lit 0x60) (Lit 0x40) (Sub Calldatasize (Lit 0x40)))
            -- Mem: [caller, callvalue, calldatasize] ++ calldata
            ,(Let "trace_size_offset" (Add Calldatasize (Lit 0x20)))
            ,(Let "trace_offset" (Add (Var "trace_size_offset") (Lit 0x20)))
            ,(Let "success1" (callHead Gas (headAddress (Lit 0)) (Lit 0) (Lit 0x00) (Var "trace_size_offset") (Lit 0x00) (Lit 0x00)))
            ,(Returndatacopy (Var "trace_size_offset") (Lit 0x20) (Sub Returndatasize (Lit 0x20)))
            -- Mem: [caller, callvalue, calldatasize] ++ calldata ++ [trace_size] ++ trace ++ returndata
            ,(Let "trace_size" (Mload (Var "trace_size_offset")))
            ,(Let "returndata_offset" (Add (Var "trace_offset") (Var "trace_size")))
            ,(Let "returndata_size" (Sub Returndatasize (Add (Lit 0x40) (Var "trace_size"))))
            ,(Let "returndata_hash" (Sha3 (Var "returndata_offset") (Var "returndata_size")))

            ,(Let "head_index" (Lit 1))
            ,(While (Lt (Var "head_index") headCount)
                  -- Call n-th head to verify trace
                  -- Input format: `[caller, callvalue, calldata_size] ++ calldata ++ [trace_size] ++ trace`
                  --
                  -- Output format:
                  -- - Success: `SUCCESS([1, trace_read] ++ returndata)`
                  -- - Failure: `FAIL([1, trace_read] ++ returndata)`
                  -- - Disagreement: `FAIL([0xd15a9])`
                  -- - OOG: `FAIL([])`
                  -- - Instrumentation Error: `FAIL([code])`
                  (Scope [(Let "success" (callHead Gas (headAddress (Var "head_index")) (Lit 0) (Lit 0x00) (Var "returndata_offset") (Lit 0x00) (Lit 0x00)))
                         ,(Assign "returndata_size" (Sub Returndatasize (Lit 0x40)))
                         ,(Returndatacopy (Var "returndata_offset") (Lit 0x40) (Var "returndata_size"))
                         ,(M.if_ (Iszero (M.and3 (Eq (Var "success1") (Var "success"))
                                                 (Eq (Var "trace_size") (returndataload (Lit 0x20)))
                                                 (Eq (Var "returndata_hash") (Sha3 (Var "returndata_offset") (Var "returndata_size")))))
                               (revertWord 0xd15a9))
                         ,(M.inc "head_index" (Lit 1))]))
            ,(Mstore (Sub (Var "returndata_offset") (Lit 0x20)) (Var "success1"))
            ,(Return (Sub (Var "returndata_offset") (Lit 0x20)) (Add (Var "returndata_size") (Lit 0x20)))])

procPayBounty = Proc "payBounty" [] "_" (Scope [err errorTODO])

-- Outer call knows three states:
-- BountyPaid, MutexOn, MutexOff (default)
procOuter = Proc "outer" [] "_" (Scope
            [(Let "outer_state" (Sload (Lit slocOuterState)))
            ,(M.if_ (Eq (Var "outer_state") (Lit outerStateBountyPaid)) (Scope [err errorTODO]))
            ,(M.if_ (Eq (Var "outer_state") (Lit outerStateMutexOn)) (Scope [err errorReentrancy]))
            ,(IfElse (Or (Eq (Var "outer_state") (Lit 0)) (Eq (Var "outer_state") (Lit outerStateMutexOff)))
                (Scope [(Sstore (Lit slocOuterState) (Lit outerStateMutexOn))])
                -- This should never happen. Pay a bounty if this happens?
                (Scope [err errorTODO]))
             -- Mutex is enabled, we're ready to call the inner MC
             -- Input format: [caller, callvalue] ++ calldata
             -- Output format:
             --   Agreement, Success => SUCCESS([0x01] ++ returndata)
             --   Agreement, Fail    => SUCCESS([0x00] ++ returndata)
             --   Disagreement       => FAIL([0xd15a9])
             --   Internal error     => Some other FAIL
            ,(Mstore (Lit 0x00) Caller)
            ,(Mstore (Lit 0x20) Callvalue)
            ,(Calldatacopy (Lit 0x40) (Lit 0x00) Calldatasize)
            ,(IfElse (Call Gas Address (Lit 0) (Lit 0x00) (Add (Lit 0x40) Calldatasize) (Lit 0x00) (Lit 0x00))
                 (Scope [(checkOrDie (M.leq (Lit 0x20) Returndatasize))
                        ,(Returndatacopy (Lit 0x00) (Lit 0x00) Returndatasize)
                        ,(IfElse (Mload (Lit 0x00))
                              (Scope [(Sstore (Lit slocOuterState) (Lit outerStateMutexOff))
                                     ,(Return (Lit 0x20) (Sub Returndatasize (Lit 0x20)))])
                              (Scope [(Revert (Lit 0x20) (Sub Returndatasize (Lit 0x20)))]))])
                 (Scope [-- This may cause an exception of there isn't enough return data.
                         -- We want an exception in this case anyways, so this is good.
                         (Returndatacopy (Lit 0x00) (Lit 0x00) (Lit 0x20))
                        ,(checkOrDie (Eq (Mload (Lit 0x00)) (Lit disagreement)))
                         -- Set state to bountyPaid and try to pay bounty
                        ,(Sstore (Lit slocOuterState) (Lit outerStateBountyPaid))
                        ,(Discard (ProcCall "payBounty" []))]))])

mc = Scope [(Let "head_address" (ProcCall "headAddress" [(Lit 0)]))
           ,(IfElse (Eq Caller (Var "head_address"))
                 (Scope [Discard (ProcCall "callback" [])])
                 (Scope [(IfElse (Eq Caller Address)
                              (Scope [(Discard (ProcCall "inner" []))])
                              (Scope [(Discard (ProcCall "outer" []))]))]))]
