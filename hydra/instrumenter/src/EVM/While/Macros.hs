module EVM.While.Macros
( if_
, add3
, and3
, geq
, leq
, die
, inc
, dec
) where

import EVM.While

if_ e s = (IfElse e s (Scope []))
add3 e1 e2 e3 = (Add e1 (Add e2 e3))
and3 e1 e2 e3 = (And e1 (And e2 e3))
geq e1 e2 = (Iszero (Lt e1 e2))
leq e1 e2 = (Iszero (Gt e1 e2))
die = Revert (Lit 0) (Lit 0)
inc v e = (Assign v (Add (Var v) e))
dec v e = (Assign v (Sub (Var v) e))