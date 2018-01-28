module EVM.While.Macros
( add3
, and3
, geq
, leq
, boom
) where

import EVM.While

if_ e s = (IfElse e s (Scope []))
add3 e1 e2 e3 = (Add e1 (Add e2 e3))
and3 e1 e2 e3 = (And e1 (And e2 e3))
geq e1 e2 = (Iszero (Lt e1 e2))
leq e1 e2 = (Iszero (Gt e1 e2))
boom = Revert (Lit 0) (Lit 0)