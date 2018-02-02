module EVM.Instrumentation.Common
where

import EVM.While
import qualified EVM.While.Macros as M

maxPrecompileAddress = 32

disagreement = 0xd15a9
revertWord w = Scope [Mstore (Lit 0x00) (Lit w)
                     ,Revert (Lit 0x00) (Lit 0x20)]

errorReentrancy = 0x1337c0ffee00000001
errorTODO       = 0x1337c0ffee00000002
errorAssert     = 0x1337c0ffee00000003
errorHeadIndexTooLarge = 0x1337c0ffee00000004
errorInputTooSmall = 0x1337c0ffee00000005
errorWrongOutputFormat = 0x1337c0ffee00000006
errorWrongInputFormat = 0x1337c0ffee00000007
errorFailedSelfCallInteracted = 0x1337c0ffee00000008
errorPrecompileCalledWithNonzeroValue = 0x1337c0ffee00000009
errorIncorrectCalldataSize = 0x1337c0ffee0000000a

-- crash code = revertWord code
--assert e = (M.if_ (Iszero e) (crash errorAssert))
--oog = Revert (Lit 0) (Lit 0)

err w = Nest (Scope [(Mstore (Lit 0x00) (Lit w))
                    ,(Revert (Lit 0x00) (Lit 0x20))])

checkOrErr w e =  (M.if_ (Iszero e)
                       (Scope [(Mstore (Lit 0x00) (Lit w))
                              ,(Revert (Lit 0x00) (Lit 0x20))]))

checkOrDie e = (M.if_ (Iszero e) (Scope [(M.die)]))

returndataload e = (ProcCall "returndataload" [e])
procReturndataload = Proc "returndataload" ["offset"] "data" (Scope
                     [(Let "backup" (Mload (Lit 0x00)))
                     ,(Returndatacopy (Lit 0x00) (Var "offset") (Lit 0x20))
                     ,(Assign "data" (Mload (Lit 0x00)))
                     ,(Mstore (Lit 0x00) (Var "backup"))
                     ])

memcpyPrecomp e1 e2 e3 = (Discard (ProcCall "memcpyPrecomp" [e1, e2, e3]))
procMemcpyPrecomp = Proc "memcpyPrecomp" ["dst", "src", "size"] "_" (Scope
                    [(checkOrDie (Call Gas (Lit 0x4) (Lit 0) (Var "src") (Var "size") (Var "dst") (Var "size")))])

memcpyNoalias e1 e2 e3 = (Discard (ProcCall "memcpyNoalias" [e1, e2, e3]))
procMemcpyNoalias = Proc "memcpyNoalias" ["dst", "src", "size"] "_" (Scope
                    [While (M.geq (Var "size") (Lit 0x20))
                         (Scope [(Mstore (Var "dst") (Mload (Var "src")))
                                ,(Assign "size" (Sub (Var "size") (Lit 0x20)))
                                ,(Assign "dst" (Add (Var "dst") (Lit 0x20)))
                                ,(Assign "src" (Add (Var "src") (Lit 0x20)))])
                    ,While (M.geq (Var "size") (Lit 0x1))
                         (Scope [(Mstore8 (Var "dst") (Byte (Lit 0) (Mload (Var "src"))))
                                ,(Assign "size" (Sub (Var "size") (Lit 0x1)))
                                ,(Assign "dst" (Add (Var "dst") (Lit 0x1)))
                                ,(Assign "src" (Add (Var "src") (Lit 0x1)))])])

min_ e1 e2 = (ProcCall "min" [e1, e2])
procMin = Proc "min" ["a", "b"] "m" (Scope
          [(IfElse (Lt (Var "a") (Var "b"))
               (Scope [(Assign "m" (Var "a"))])
               (Scope [(Assign "m" (Var "b"))]))])

callHead e1 e2 e3 e4 e5 e6 e7 = ProcCall "callHead" [e1,e2,e3,e4,e5,e6,e7]
procCallHead = Proc "callHead" ["gas", "to", "value", "in_offset", "in_size", "out_offset", "out_size"] "success" (Scope
               [(Assign "success" (Call (Var "gas") (Var "to") (Var "value") (Var "in_offset") (Var "in_size") (Var "out_offset") (Var "out_size")))
               -- propagate OOG, disagreement, and instrumentation errors
               ,(M.if_ (Iszero (Var "success"))
                     (Scope [(Let "first_word" (returndataload (Lit 0x00)))
                            ,(M.if_ (Eq (Var "first_word") (Lit 0xd15a9)) (revertWord 0xd15a9))
                            ,(M.if_ (M.leq Returndatasize (Lit 0x20)) (Scope [(Revert (Lit 0x00) (Lit 0x00))]))]))
               ,(checkOrErr errorWrongOutputFormat (Eq (returndataload (Lit 0x00)) (Lit 1)))])


procMc mc = Proc "mc" [] "address" (Scope [(Assign "address" (Lit mc))])

procUnknownJumpdest = Proc "unknownJumpdest" [] "_" (Scope
                      [(Discard (Lit 314159265358979)), (Discard (ProcCall "done" [Lit 0, Lit 0, Lit 0]))])


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
