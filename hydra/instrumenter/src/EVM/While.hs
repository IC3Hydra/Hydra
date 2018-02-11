module EVM.While
( Expr (..)
, Stmt (..)
, Scope (..)
, Proc (..)
, compExpr
, compStmt
, compScope
, compProc
, compile
, compileAndLower
, procTag
) where

import Data.Maybe
import Data.List
import Data.Word
import EVM.Bytecode (Opcode(..))
import EVM.BytecodePlus (OpcodePlus(..), lower)
import Prelude hiding (EQ, GT, LT)
import Text.Printf
import Util

data Expr = Lit Integer
          | Var String
          | ProcCall String [Expr]
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Sdiv Expr Expr
          | Mod Expr Expr
          | Smod Expr Expr
          -- ADDMOD
          -- MULMOD
          -- EXP
          -- SIGNEXTEND
          | Lt Expr Expr
          | Gt Expr Expr
          -- SLT
          -- SGT
          | Eq Expr Expr
          | Iszero Expr
          | And Expr Expr
          | Or Expr Expr
          | Xor Expr Expr
          | Not Expr
          | Byte Expr Expr
          | Sha3 Expr Expr
          | Address
          | Balance Expr
          | Origin
          | Caller
          | Callvalue
          | Calldataload Expr
          | Calldatasize
          -- CALLDATACOPY is a statement
          -- CODESIZE
          -- CODECOPY
          -- GASPRICE
          -- EXTCODESIZE
          -- EXTCODECOPY
          | Returndatasize
          -- RETURNDATACOPY is a statement
          -- BLOCKHASH
          -- COINBASE
          -- TIMESTAMP
          -- NUMBER
          -- DIFFICULTY
          -- GASLIMIT
          -- POP: no need for manual stack management :)
          | Mload Expr
          -- MSTORE is a statement
          -- MSTORE8 is a statement
          | Sload Expr
          -- SSTORE is a statement
          -- JUMP: no need for manual control flow
          -- JUMPI: no need for manual control flow
          -- PC
          | Msize
          | Gas
          -- JUMPDEST: no need for manual control flow
          -- PUSH Word8 Integer: no need for manual stack management
          -- DUP Word8: no need for manual stack management
          -- SWAP Word8: no need for manual stack management
          -- Log0 is a statement
          -- Log1 is a statement
          -- Log2 is a statement
          -- Log3 is a statement
          -- Log4 is a statement
          -- CREATE
          | Call Expr Expr Expr Expr Expr Expr Expr
          -- CALLCODE
          -- RETURN is a statement
          -- DELEGATECALL
          -- STATICCALL
          -- REVERT is a statement
          -- SUICIDE
          -- Unknown Word8
    deriving (Show, Eq)

data Stmt = Let String Expr
          | Assign String Expr
          | Discard Expr
          | Nest Scope
          | IfElse Expr Scope Scope
          | While Expr Scope
          | Calldatacopy Expr Expr Expr
          | Returndatacopy Expr Expr Expr
          | Mstore Expr Expr
          | Mstore8 Expr Expr
          | Sstore Expr Expr
          | Log0 Expr Expr
          | Log1 Expr Expr Expr
          | Log2 Expr Expr Expr Expr
          | Log3 Expr Expr Expr Expr Expr
          | Log4 Expr Expr Expr Expr Expr Expr
          | Return Expr Expr
          | Revert Expr Expr
    deriving (Show, Eq)

data Scope = Scope [Stmt]
    deriving (Show, Eq)

data Proc = Proc String [String] String Scope
    deriving (Show, Eq)

stackIndex :: (Show a, Eq a) => [a] -> a -> Word8
stackIndex ss s = let i = (justOrError (printf "Unknown stack item %v" $ show s) (elemIndex s ss)) + 1
                  in if i <= 16 then fromIntegral i else error ("variable out of reach:" ++ show ss ++ show s)

prog = Scope [(Let "psize" (Mload (Lit 0)))
             ,(Let "pcap" (Mload (Lit 1)))
             ,(IfElse (Lt (Var "pcap") (Add (Add (Var "psize") (Var "logsize")) (Lit 2)))
                      (Scope [])
                      (Scope []))]

compExprAux :: [Maybe String] -> Expr -> ([Maybe String], [OpcodePlus])
compExprAux ss e =
    case e of
        (Lit i) -> (Nothing : ss, [Push i])
        (Var s) -> (Nothing : ss, [Op $ DUP $ stackIndex ss (Just s)])
        -- TODO(lorenzb): There should be a check to ensure that the procedure actually exists.
        (ProcCall s es) -> compExprAuxn ss [ProcedureCall $ printf "proc_%s" s] es
        (Add e1 e2) -> aux2 ss ADD e1 e2
        (Mul e1 e2) -> aux2 ss MUL e1 e2
        (Sub e1 e2) -> aux2 ss SUB e1 e2
        (Div e1 e2) -> aux2 ss DIV e1 e2
        (Sdiv e1 e2) -> aux2 ss SDIV e1 e2
        (Mod e1 e2) -> aux2 ss MOD e1 e2
        (Smod e1 e2) -> aux2 ss SMOD e1 e2
        (Lt e1 e2) -> aux2 ss LT e1 e2
        (Gt e1 e2) -> aux2 ss GT e1 e2
        (Eq e1 e2) -> aux2 ss EQ e1 e2
        (Iszero e) -> aux1 ss ISZERO e
        (And e1 e2) -> aux2 ss AND e1 e2
        (Or e1 e2) -> aux2 ss OR e1 e2
        (Xor e1 e2) -> aux2 ss XOR e1 e2
        (Not e) -> aux1 ss NOT e
        (Byte e1 e2) -> aux2 ss BYTE e1 e2
        (Sha3 e1 e2) -> aux2 ss SHA3 e1 e2
        Address -> aux0 ss ADDRESS
        (Balance e) -> aux1 ss BALANCE e
        Origin -> aux0 ss ORIGIN
        Caller -> aux0 ss CALLER
        Callvalue -> aux0 ss CALLVALUE
        (Calldataload e) -> aux1 ss CALLDATALOAD e
        Calldatasize -> aux0 ss CALLDATASIZE
        Returndatasize -> aux0 ss RETURNDATASIZE
        (Mload e) -> aux1 ss MLOAD e
        (Sload e) -> aux1 ss SLOAD e
        Msize -> aux0 ss MSIZE
        Gas -> aux0 ss GAS
        (Call e1 e2 e3 e4 e5 e6 e7) -> aux7 ss CALL e1 e2 e3 e4 e5 e6 e7
    where aux0 ss op = compExprAuxn ss [Op op] []
          aux1 ss op e = compExprAuxn ss [Op op] [e]
          aux2 ss op e1 e2 = compExprAuxn ss [Op op] [e1, e2]
          aux3 ss op e1 e2 e3 = compExprAuxn ss [Op op] [e1, e2, e3]
          aux7 ss op e1 e2 e3 e4 e5 e6 e7 = compExprAuxn ss [Op op] [e1, e2, e3, e4, e5, e6, e7]

compExprAuxn :: [Maybe String] -> [OpcodePlus] -> [Expr] -> ([Maybe String], [OpcodePlus])
compExprAuxn ss ops es = let f = (\(ss, ops) e -> let (ss', ops') = compExprAux ss e in (ss', ops ++ ops'))
                             (ss', ops') = foldl f (ss, []) (reverse es)
                         in (Nothing:drop (length es) ss', ops' ++ ops)

-- Compiles an expression.
-- Takes symbolic stack and expression.
-- Returns opcodes.
-- You may assume that exactly one item is added on top of the
-- stack after executing the resulting opcodes.
compExpr :: [String] -> Expr -> [OpcodePlus]
compExpr ss e = let (_, ops) = compExprAux (map Just ss) e in ops


compStmtAuxn :: [String] -> Opcode -> [Expr] -> [OpcodePlus]
compStmtAuxn ss op es = let f = (\(ss, ops) e -> let (ss', ops') = compExprAux ss e in (ss', ops ++ ops'))
                            (ss', ops) = foldl f (map Just ss, []) (reverse es)
                        in ops ++ [Op op]

-- Compiles a statment.
-- Takes inner symbolic stack (i.e. symbolic stack inside current scope),
-- outer symbolic stack (i.e. symbolic stack outside current scope), and
-- statement.
-- Returns updated inner stack (needed for assignments) and opcodes.
compStmt :: [String] -> [String] -> String -> Stmt -> ([String], [OpcodePlus])
compStmt iss oss tagpre = aux
    where ss = iss ++ oss
          aux (Let s e) = if s `elem` ss
                          then error $ printf "Can't let %s. Already exists." s
                          else (s:iss, compExpr ss e)
          aux (Assign s e) = if s `elem` ss
                          then (iss, compExpr ss e ++ [Op $ SWAP $ stackIndex ss s, Op POP])
                          else error $ printf "Can't assign %s. Doesn't exist." s
          aux (Discard e) = (iss, compExpr ss e ++ [Op $ POP])
          aux (Nest s) = (iss, compScope ss (tagpre ++ "_scope") s)
          aux (IfElse e s1 (Scope [])) = (iss,  compExpr ss (Iszero e)
                                             ++ [TagJumpi $ label "endif"]
                                             ++ compScope ss (tagpre ++ "_ifscope") s1
                                             ++ [TagJumpdest $ label "endif"])
          aux (IfElse e s1 s2) = (iss,  compExpr ss e
                                     ++ [TagJumpi $ label "if"]
                                     ++ compScope ss (tagpre ++ "_elsescope") s2
                                     ++ [TagJump $ label "endif", TagJumpdest $ label "if"]
                                     ++ compScope ss (tagpre ++ "_ifscope") s1
                                     ++ [TagJumpdest $ label "endif"])
          aux (While e s) = (iss,  [TagJumpdest $ label "while"]
                                ++ compExpr ss e
                                ++ [Op ISZERO, TagJumpi $ label "endwhile"]
                                ++ compScope ss (tagpre ++ "_whilescope") s
                                ++ [TagJump $ label "while", TagJumpdest $ label "endwhile"])
          aux (Calldatacopy e1 e2 e3) = (iss, compStmtAuxn ss CALLDATACOPY [e1, e2, e3])
          aux (Returndatacopy e1 e2 e3) = (iss, compStmtAuxn ss RETURNDATACOPY [e1, e2, e3])
          aux (Mstore e1 e2) = (iss, compStmtAuxn ss MSTORE [e1, e2])
          aux (Mstore8 e1 e2) = (iss, compStmtAuxn ss MSTORE8 [e1, e2])
          aux (Sstore e1 e2) = (iss, compStmtAuxn ss SSTORE [e1, e2])
          aux (Log0 e1 e2) = (iss, compStmtAuxn ss LOG0 [e1, e2])
          aux (Log1 e1 e2 e3) = (iss, compStmtAuxn ss LOG1 [e1, e2, e3])
          aux (Log2 e1 e2 e3 e4) = (iss, compStmtAuxn ss LOG2 [e1, e2, e3, e4])
          aux (Log3 e1 e2 e3 e4 e5) = (iss, compStmtAuxn ss LOG3 [e1, e2, e3, e4, e5])
          aux (Log4 e1 e2 e3 e4 e5 e6) = (iss, compStmtAuxn ss LOG4 [e1, e2, e3, e4, e5, e6])
          aux (Return e1 e2) = (iss, compStmtAuxn ss RETURN [e1, e2])
          aux (Revert e1 e2) = (iss, compStmtAuxn ss REVERT [e1, e2])
          label s = printf "%s_%s" tagpre s

-- Compiles a scope.
-- The pre- and post-scope symbolic stacks are equal.
compScope :: [String] -> String -> Scope -> [OpcodePlus]
compScope oss tagpre (Scope stmts) = let (iss, ops) = foldl aux ([], []) (zip stmts [0..])
                                     in ops ++ map (const (Op POP)) iss
    where aux :: ([String], [OpcodePlus]) -> (Stmt, Int) -> ([String], [OpcodePlus])
          aux (iss, ops) (stmt, i) = let (iss', ops') = compStmt iss oss (printf "%s_%d" tagpre i) stmt
                                     in (iss', ops ++ ops')

procTag :: String -> String
procTag name = printf "proc_%s" name

compProc :: Proc -> [OpcodePlus]
compProc (Proc name params output scope) =
    [TagJumpdest $ procTag name]
    -- stack is [return_pc, param1, param2, param3, ...]
    ++ swap (length params)
    -- stack is [paramn, param1, param2, param3, ..., paramn-1, return_pc]
    ++ [Push 0]
    -- stack is [output, paramn, param1, param2, param3, ..., paramn-1, return_pc]
    ++ compScope (output:rotateRight params) (procTag name) scope
    -- stack is [output, paramn, param1, param2, param3, ..., paramn-1, return_pc]
    ++ swap (length params)
    -- stack is [paramn-1, paramn, param1, param2, param3, ..., output, return_pc]
    ++ map (const $ Op POP) params
    -- stack is [output, return_pc]
    ++ swap 1
    -- stack is [return_pc, output]
    ++ [Op JUMP]
    -- stack is [output]
    where swap i | i == 0    = []
                 | i <= 16   = [Op $ SWAP $ fromIntegral i]
                 | otherwise = error (printf "Proc %s has too many params" name)
          rotateRight [] = []
          rotateRight xs = last xs : init xs

compile :: [Proc] -> Scope -> [OpcodePlus]
compile procs main = compScope [] "" main ++ [Op STOP] ++ (concat $ map compProc procs)

compileAndLower :: [Proc] -> Scope -> Either String [Opcode]
compileAndLower procs main = lower $ compile procs main
