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
procReturndataload = Proc "returndataload" ["offset"] "data" (Scope
                     [(Let "backup" (Mload (Lit 0x00)))
                     ,(Returndatacopy (Lit 0x00) (Var "offset") (Lit 0x20))
                     ,(Assign "data" (Mload (Lit 0x00)))
                     ,(Mstore (Lit 0x00) (Var "backup"))
                     ])

returndataload e = (ProcCall "returndataload" [e])