module EVM.Instrumentation.Metacontract
( metacontract
) where

import EVM.Bytecode (Opcode)
import EVM.While
import qualified EVM.While.Macros as M
import Util

metacontract :: [Integer] -> [Opcode]
metacontract heads = fromRight $ compileAndLower procs mc
    where procs = [ procHeadAddress heads
                  , procHeadCount heads
                  , procCallback
                  , procInner
                  , procPayBounty
                  , procOuter
                  ]

slocOuterState = 0
outerStateBountyPaid = 1
outerStateMutexOn = 2
outerStateMutexOff = 3

disagreement = 0xd15a9
revertWord w = Scope [Mstore (Lit 0x00) (Lit w)
                     ,Revert (Lit 0x00) (Lit 0x20)]

errorReentrancy = 0x1337c0ffee00000001
errorTODO       = 0x1337c0ffee00000002
errorAssert     = 0x1337c0ffee00000003
crash code = revertWord code
assert e = (M.if_ (Iszero e) (crash errorAssert))
oog = Revert (Lit 0) (Lit 0)

procHeadAddress heads = Proc "headAddress" ["index"] "address" (Scope $
                        [(assert (Lt (Var "index") (Lit (fromIntegral $ length heads))))
                        ] ++ zipWith (\i a -> (M.if_ (Eq (Var "index") (Lit i)) (Scope [Assign "address" (Lit a)]))) [0..] heads)

procHeadCount heads = Proc "headCount" [] "count" (Scope [(Assign "count" (Lit (fromIntegral $ length heads)))])

procCallback = Proc "callback" [] "_" (Scope
               [(assert (M.leq (Lit 0x20) Calldatasize))
               ,(Let "type" (Calldataload (Sub Calldatasize (Lit 0x20))))
               ,(IfElse (M.leq (Var "type") (Lit 4))
                     -- LOG
                     (Scope [(Let "delta" (Lit 0x40))
                            ,(Let "topic1" (Lit 0))
                            ,(Let "topic2" (Lit 0))
                            ,(Let "topic3" (Lit 0))
                            ,(Let "topic4" (Lit 0))
                            ,(M.if_ (M.leq (Lit 4) (Var "type"))
                                  (Scope [(Assign "topic4" (Calldataload (Sub Calldatasize (Var "delta"))))
                                         ,(Assign "delta" (Add (Var "delta") (Lit 0x20)))]))
                            ,(M.if_ (M.leq (Lit 3) (Var "type"))
                                  (Scope [(Assign "topic3" (Calldataload (Sub Calldatasize (Var "delta"))))
                                         ,(Assign "delta" (Add (Var "delta") (Lit 0x20)))]))
                            ,(M.if_ (M.leq (Lit 2) (Var "type"))
                                  (Scope [(Assign "topic2" (Calldataload (Sub Calldatasize (Var "delta"))))
                                         ,(Assign "delta" (Add (Var "delta") (Lit 0x20)))]))
                            ,(M.if_ (M.leq (Lit 1) (Var "type"))
                                  (Scope [(Assign "topic1" (Calldataload (Sub Calldatasize (Var "delta"))))
                                         ,(Assign "delta" (Add (Var "delta") (Lit 0x20)))]))
                            ,(Let "logdatasize" (Sub Calldatasize (Add (Lit 0x20) (Mul (Var "type") (Lit 0x20)))))
                            ,(Calldatacopy (Lit 0x00) (Lit 0x00) (Var "logdatasize"))
                            ,(M.if_ (Eq (Var "type") (Lit 0)) (Scope [(Log0 (Lit 0x00) (Var "logdatasize"))]))
                            ,(M.if_ (Eq (Var "type") (Lit 1)) (Scope [(Log1 (Lit 0x00) (Var "logdatasize") (Var "topic1"))]))
                            ,(M.if_ (Eq (Var "type") (Lit 2)) (Scope [(Log2 (Lit 0x00) (Var "logdatasize") (Var "topic1") (Var "topic2"))]))
                            ,(M.if_ (Eq (Var "type") (Lit 3)) (Scope [(Log3 (Lit 0x00) (Var "logdatasize") (Var "topic1") (Var "topic2") (Var "topic3"))]))
                            ,(M.if_ (Eq (Var "type") (Lit 4)) (Scope [(Log4 (Lit 0x00) (Var "logdatasize") (Var "topic1") (Var "topic2") (Var "topic3") (Var "topic4"))]))])
                     -- CALL
                     (Scope [(assert (Eq (Var "type") (Lit 5)))
                            ,(Let "datasize" (Sub Calldatasize (Lit 0x60)))
                            ,(Calldatacopy (Lit 0x00) (Lit 0x00) (Var "datasize"))
                            ,(Let "to" (Calldataload (Var "datasize")))
                            ,(Let "value" (Calldataload (Add (Var "datasize") (Lit 0x20))))
                            ,(Let "success" (Call Gas (Var "to") (Var "value") (Lit 0x00) (Var "datasize") (Lit 0x00) (Lit 0x00)))
                            ,(Mstore (Lit 0x00) (Var "success"))
                            ,(Returndatacopy (Lit 0x20) (Lit 0x00) Returndatasize)
                            ,(Return (Lit 0x00) (Add Returndatasize (Lit 0x20)))]))])

-- TODO(lorenzb): Compare return values from different heads?
procInner = Proc "inner" [] "_" (Scope
            [(assert (M.leq (Lit 0x40) Calldatasize))
            ,(Let "caller" (Calldataload (Lit 0x00)))
            ,(Let "callvalue" (Calldataload (Lit 0x20)))
            -- Call first head to construct trace
            -- Input format: [caller, callvalue] ++ calldata
            -- Output format:
            -- Success or Failure: SUCCESS([trace_length] ++ trace ++ [success] ++ returndata)
            -- OOG: FAIL([])
            ,(Let "head_address" (ProcCall "headAddress" [(Lit 0)]))
            ,(Mstore (Lit 0x00) (Var "caller"))
            ,(Mstore (Lit 0x20) (Var "callvalue"))
            ,(Calldatacopy (Lit 0x40) (Lit 0x40) Calldatasize)
            ,(IfElse (Call Gas (Var "head_address") (Lit 0) (Lit 0x00) (Add Calldatasize (Lit 0x40)) (Lit 0x00) (Lit 0x00))
                  (Scope [(Returndatacopy (Lit 0x00) (Lit 0x00) Returndatasize)])
                  (Scope [oog]))
            ,(Let "trace_length" (Mload (Lit 0x00)))
            ,(Let "returndatasize" (Sub Returndatasize (Add (Var "trace_length") (Lit 0x40))))
            ,(Let "head_index" (Lit 1))
            ,(While (Lt (Var "head_index") (ProcCall "headCount" []))
                  (Scope [(Assign "head_address" (ProcCall "headAddress" [(Var "head_index")]))
                          -- Call n-th head to verify trace
                          -- Input format: trace
                          -- Output format:
                          -- Success: SUCCESS([1])
                          -- Failure (disagreement): FAIL([0xd15a9])
                          -- OOG: FAIL([])
                         ,(Let "success" (Call Gas (Var "head_address") (Lit 0) (Lit 0x20) (Var "trace_length") (Lit 0x00) (Lit 0x00)))
                         ,(assert (Eq Returndatasize (Lit 0x20)))
                         ,(Returndatacopy (Lit 0x00) (Lit 0x00) (Lit 0x20))
                         ,(Let "output" (Mload (Lit 0x00)))
                         ,(IfElse (Var "success")
                               (Scope [(assert (Eq (Var "output") (Lit 1)))])
                               (Scope [(assert (Eq (Var "output") (Lit disagreement)))
                                      ,(Nest (revertWord disagreement))]))
                         ,(Assign "head_index" (Add (Var "head_index") (Lit 1)))]))
             -- If we reach this point, all heads agreed.
            ,(Let "success" (Mload (Add (Var "trace_length") (Lit 0x20))))
            ,(IfElse (Var "success")
                  (Scope [Return (Add (Var "trace_length") (Lit 0x40)) (Var "returndatasize")])
                  (Scope [Revert (Add (Var "trace_length") (Lit 0x40)) (Var "returndatasize")]))])

procPayBounty = Proc "payBounty" [] "_" (crash errorTODO)

-- Outer call knows three states:
-- BountyPaid, MutexOn, MutexOff (default)
procOuter = Proc "outer" [] "_" (Scope
            [(Let "outer_state" (Sload (Lit slocOuterState)))
            ,(M.if_ (Eq (Var "outer_state") (Lit outerStateBountyPaid)) (crash errorTODO))
            ,(M.if_ (Eq (Var "outer_state") (Lit outerStateMutexOn)) (crash errorReentrancy))
            ,(IfElse (Or (Eq (Var "outer_state") (Lit 0)) (Eq (Var "outer_state") (Lit outerStateMutexOff)))
                (Scope [(Sstore (Lit slocOuterState) (Lit outerStateMutexOn))])
                -- This should never happen. Pay a bounty if this happens?
                (crash errorTODO))
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
                 (Scope [(assert (M.leq (Lit 0x20) Returndatasize))
                        ,(Returndatacopy (Lit 0x00) (Lit 0x00) Returndatasize)
                        ,(IfElse (Mload (Lit 0x00))
                              (Scope [(Sstore (Lit slocOuterState) (Lit outerStateMutexOff))
                                     ,(Return (Lit 0x20) (Sub Returndatasize (Lit 0x20)))])
                              (Scope [(Revert (Lit 0x20) (Sub Returndatasize (Lit 0x20)))]))])
                 (Scope [-- This may cause an exception of there isn't enough return data.
                         -- We want an exception in this case anyways, so this is good.
                         (Returndatacopy (Lit 0x00) (Lit 0x00) (Lit 0x20))
                        ,(assert (Eq (Mload (Lit 0x00)) (Lit disagreement)))
                         -- Set state to bountyPaid and try to pay bounty
                        ,(Sstore (Lit slocOuterState) (Lit outerStateBountyPaid))
                        ,(Assign "_" (ProcCall "payBounty" []))]))])

mc = Scope [(Let "_" (Lit 0))
           ,(Let "head_address" (ProcCall "headAddress" [(Lit 0)]))
           ,(IfElse (Eq Caller (Var "head_address"))
                 (Scope [Assign "_" (ProcCall "callback" [])])
                 (Scope [(IfElse (Eq Caller Address)
                              (Scope [(Assign "_" (ProcCall "inner" []))])
                              (Scope [(Assign "_" (ProcCall "outer" []))]))]))]
--           ,(if_ (Eq (Lit mutexOn) (Sload slocMutex)) (Scope (crash errorReentrancy)))
--           ,(Sstore (Lit slocMutex) (Lit mutexOn))
