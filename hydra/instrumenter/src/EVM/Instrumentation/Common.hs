module EVM.Instrumentation.Common
where

import EVM.While
import qualified EVM.While.Macros as M

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
-- crash code = revertWord code
--assert e = (M.if_ (Iszero e) (crash errorAssert))
--oog = Revert (Lit 0) (Lit 0)

err w = Nest (Scope [(Mstore (Lit 0x00) (Lit w))
                    ,(Revert (Lit 0x00) (Lit 0x20))])

checkOrErr w e =  (M.if_ (Iszero e)
                       (Scope [(Mstore (Lit 0x00) (Lit w))
                              ,(Revert (Lit 0x00) (Lit 0x20))]))

checkOrDie e = (M.if_ (Iszero e) (Scope [(Revert (Lit 0x00) (Lit 0x00))]))

returndataload e = (ProcCall "returndataload" [e])
procReturndataload = Proc "returndataload" ["offset"] "data" (Scope
                     [(Let "backup" (Mload (Lit 0x00)))
                     ,(Returndatacopy (Lit 0x00) (Var "offset") (Lit 0x20))
                     ,(Assign "data" (Mload (Lit 0x00)))
                     ,(Mstore (Lit 0x00) (Var "backup"))
                     ])

memcpyPrecomp e1 e2 e3 = (Discard (ProcCall "memcpyPrecomp" [e1, e2, e3]))
procMemcpyPrecomp = Proc "memcpyPrecomp" ["dst", "src", "size"] "_" (Scope
                    [(IfElse (Call Gas (Lit 0x4) (Lit 0) (Var "src") (Var "size") (Var "dst") (Var "size"))
                          (Scope [])
                          (Scope [(M.boom)]))])

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

