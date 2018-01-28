module EVM.Instrumentation.Internal
where

import           Data.List
import           EVM.Bytecode
import           EVM.BytecodePlus
import           EVM.While
import qualified EVM.While.Macros as M
import           Prelude          hiding (EQ, GT, LT)
import           Text.Printf
import           Util

instrumentOps :: Integer -> [OpcodePlus] -> [OpcodePlus]
instrumentOps mc = concatMap aux
    where aux (Op ADDRESS)      = [ Push mc    -- ⤳ S [N "mc_address"]
                                  ]
          aux (Op BALANCE)      = [ ProcedureCall $ procTag "balance" -- ⤳ S [N "balance"]
                                  ]
          aux (Op CALLER)       = [ Push 0           -- ⤳ S [N "0"]
                                  , Op $ CALLDATALOAD     -- ⤳ S [N "calldata[0]"]
                                  ]
          aux (Op CALLVALUE)    = [ Push 0x20       -- ⤳ S [N "0x20"]
                                  , Op $ CALLDATALOAD    -- ⤳ S [N "calldata[0x20]"]
                                  ]
          aux (Op CALLDATALOAD) = [ Push calldataStashSize -- ⤳ S [N "calldata stash size", V "offset"]
                                  , Op $ ADD                     -- ⤳ S [N "offset + calldata stash size"]
                                  , Op $ CALLDATALOAD            -- ⤳ S [N "C[offset + calldata stash size]"]
                                  ]
          aux (Op CALLDATASIZE) = [ Op $ CALLDATASIZE           -- ⤳ S [N "calldata size"]
                                  , Push calldataStashSize -- ⤳ S [N "calldata stash size", V "calldata size"]
                                  , Op $ (SWAP  1)               -- ⤳ S [V "calldata size", V "calldata stash size"]
                                  , Op $ SUB                     -- ⤳ S [N "calldata size - calldata stash size"]
                                  ]
          aux (Op CALLDATACOPY) = [ Push memoryStashSize   -- ⤳ S [N "memory stash size", V "dst offset", V "src offset", V "size"]
                                  , Op $ ADD                     -- ⤳ S [N "dst offset + memory stash size", V "src offset", V "size"]
                                  , Op $ (SWAP  1)               -- ⤳ S [V "src offset", V "dst offset + memory stash size", V "size"]
                                  , Push calldataStashSize -- ⤳ S [N "calldata stash size", V "src offset", V "dst offset + memory stash size", V "size"]
                                  , Op $ ADD                     -- ⤳ S [N "src offset + calldata stash size", V "dst offset + memory stash size", V "size"]
                                  , Op $ (SWAP  1)               -- ⤳ S [V "dst offset + memory stash size", V "src offset + calldata stash size", V "size"]
                                  , Op $ CALLDATACOPY            -- ⤳ S []
                                  ]
          aux (Op MLOAD)        = [ Push memoryStashSize -- ⤳ S [N "memory stash size", V "offset"]
                                  , Op $ ADD                   -- ⤳ S [N "offset + memory stash size"]
                                  , Op $ MLOAD                 -- ⤳ S [N "M[offset + memory stash size]"]
                                  ]
          aux (Op MSTORE)       = [ Push memoryStashSize -- ⤳ S [N "memory stash size", V "offset", V "word"]
                                  , Op $ ADD                   -- ⤳ S [N "offset + memory stash size", V "word"]
                                  , Op $ MSTORE                -- ⤳ S []
                                  ]
          aux (Op MSTORE8)      = [ Push memoryStashSize -- ⤳ S [N "memory stash size", V "offset", V "byte"]
                                  , Op $ ADD                   -- ⤳ S [N "offset + memory stash size", V "byte"]
                                  , Op $ MSTORE8              -- ⤳ S []
                                  ]
          aux (Op SHA3)         = [ Push memoryStashSize -- ⤳ S [N "memory stash size", V "offset", V "size"]
                                  , Op $ ADD                   -- ⤳ S [N "offset + memory stash size", V "size"]
                                  , Op $ SHA3                 -- ⤳ S [N "H(M[offset + memory stash size : offset + memory stash size + size])"]
                                  ]
          aux (Op JUMP)         = [ TagJump "jumptable"  -- ⤳ Ø
                                  ]
          aux (Op JUMPI)        = [ Op $ (SWAP 1)             -- ⤳ S [V "should_jump", V "pc"]
                                  , TagJumpi "jumptable" -- ⤳ S [V "pc"]
                                  , Op $ POP                   -- ⤳ S []
                                  ]
          aux (Op MSIZE)        = [ Op $ MSIZE                -- ⤳ S [N "msize"]
                                  , Push memoryStashSize -- ⤳ S [N "memory stash size", V "msize"]
                                  , Op $ (SWAP  1)             -- ⤳ S [V "msize", V "memory stash size"]
                                  , Op $ SUB                   -- ⤳ S [N "msize - memory stash size"]
                                  ]
          aux (Op LOG0)         = [ ProcedureCall $ procTag "log0" -- ⤳ S []
                                  , Op POP
                                  ]
          aux (Op LOG1)         = [ ProcedureCall $ procTag "log1" -- ⤳ S []
                                  , Op POP
                                  ]
          aux (Op LOG2)         = [ ProcedureCall $ procTag "log2" -- ⤳ S []
                                  , Op POP
                                  ]
          aux (Op LOG3)         = [ ProcedureCall $ procTag "log3" -- ⤳ S []
                                  , Op POP
                                  ]
          aux (Op LOG4)         = [ ProcedureCall $ procTag "log4" -- ⤳ S []
                                  , Op POP
                                  ]
          aux (Op CALL)         = [ ProcedureCall $ procTag "call" -- ⤳ S [N "success"]
                                  ]
          aux (Op RETURN)       = [ ProcedureCall $ procTag "return" -- ⤳ Ø
                                  ]
          aux (Op (Unknown _))  = [ ProcedureCall $ procTag "die" -- ⤳ Ø
                                  ]
          aux op                = [ op ]



{-

link (InstrumentedContract procs jt contract) = [ ProcedureCall "init" ]
                                                ++ contract
                                                ++ [ Op STOP ]
                                                ++ concatMap aux procs
                                                ++ jt
    where aux (Procedure name _ _ opswithstacks) = map getOp opswithstacks
          getOp (OpcodePlusWithState op stack) = op


procs :: Integer -> [Procedure]
procs mc = [ procBalance
           , procCall
           , procDie
           , procInit
           , procMcAddress mc
           , procMemcpyExcl
           , procLog
           , procReturn
           ]
-}


procs :: Integer -> [Proc]
procs mc = [ procMemcpy
          , procMemcpy2
          , procMin
          , procLog0
          , procLog1
          , procLog2
          , procLog3
          , procLog4
          , procLog
          , procCall
          , procBalance
          , procReturn
          , procInit
          , procDie
          , procMc mc
          ]

memoryStashSize = 9 * 0x20

calldataStashSize = 2 * 0x20

maxPrecompileAddress = 16

procMemcpy = Proc "memcpy" ["dst", "src", "size"] "_" (Scope
             [(IfElse (Call Gas (Lit 0x4) (Lit 0) (Var "src") (Var "size") (Var "dst") (Var "size"))
                   (Scope [])
                   (Scope [(M.boom)]))])

procMemcpy2 = Proc "memcpy2" ["dst", "src", "size"] "_" (Scope
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

procMin = Proc "min" ["a", "b"] "m" (Scope
          [(IfElse (Lt (Var "a") (Var "b"))
               (Scope [(Assign "m" (Var "a"))])
               (Scope [(Assign "m" (Var "b"))]))])

procLog0 = Proc "log0" ["in_offset", "in_size"] "_" (Scope
           [Assign "_" (ProcCall "log" [(Lit 0), (Var "in_offset"), (Var "in_size"), (Lit 0), (Lit 0), (Lit 0), (Lit 0)])])

procLog1 = Proc "log1" ["in_offset", "in_size", "topic1"] "_" (Scope
           [Assign "_" (ProcCall "log" [(Lit 1), (Var "in_offset"), (Var "in_size"), (Var "topic1"), (Lit 0), (Lit 0), (Lit 0)])])

procLog2 = Proc "log2" ["in_offset", "in_size", "topic1", "topic2"] "_" (Scope
           [Assign "_" (ProcCall "log" [(Lit 2), (Var "in_offset"), (Var "in_size"), (Var "topic1"), (Var "topic2"), (Lit 0), (Lit 0)])])

procLog3 = Proc "log3" ["in_offset", "in_size", "topic1", "topic2", "topic3"] "_" (Scope
           [Assign "_" (ProcCall "log" [(Lit 3), (Var "in_offset"), (Var "in_size"), (Var "topic1"), (Var "topic2"), (Var "topic3"), (Lit 0)])])

procLog4 = Proc "log4" ["in_offset", "in_size", "topic1", "topic2", "topic3", "topic4"] "_" (Scope
           [Assign "_" (ProcCall "log" [(Lit 4), (Var "in_offset"), (Var "in_size"), (Var "topic1"), (Var "topic2"), (Var "topic3"), (Var "topic4")])])

procLog = Proc "log" ["num_topics", "in_offset", "in_size", "topic1", "topic2", "topic3", "topic4"] "_" (Scope
           [(Assign "in_offset" (Add (Var "in_offset") (Lit memoryStashSize)))
           ,(Assign "_" (ProcCall "memcpy" [(Lit 0), (Add (Var "in_offset") (Var "in_size")), (Mul (Lit 0x20) (Add (Lit 1) (Var "num_topics")))]))
           ,(IfElse (Lt (Lit 0) (Var "num_topics")) (Scope [(Mstore (M.add3 (Var "in_offset") (Var "in_size") (Lit 0x00)) (Var "topic1"))]) (Scope []))
           ,(IfElse (Lt (Lit 1) (Var "num_topics")) (Scope [(Mstore (M.add3 (Var "in_offset") (Var "in_size") (Lit 0x20)) (Var "topic2"))]) (Scope []))
           ,(IfElse (Lt (Lit 2) (Var "num_topics")) (Scope [(Mstore (M.add3 (Var "in_offset") (Var "in_size") (Lit 0x40)) (Var "topic3"))]) (Scope []))
           ,(IfElse (Lt (Lit 3) (Var "num_topics")) (Scope [(Mstore (M.add3 (Var "in_offset") (Var "in_size") (Lit 0x60)) (Var "topic4"))]) (Scope []))
           ,(Mstore (M.add3 (Var "in_offset") (Var "in_size") (Mul (Lit 0x20) (Var "num_topics"))) (Var "num_topics"))
           ,(IfElse (Call Gas (ProcCall "mc" []) (Lit 0) (Var "in_offset") (Add (Var "in_size") (Mul (Lit 0x20) (Add (Lit 1) (Var "num_topics")))) (Lit 0) (Lit 0))
                 (Scope [(Assign "_" (ProcCall "memcpy" [(Add (Var "in_offset") (Var "in_size")), (Lit 0), (Mul (Lit 0x20) (Add (Lit 1) (Var "num_topics")))]))])
                 (Scope [(M.boom)]))
            ])

procCall = Proc "call" ["gas", "to", "value", "in_offset", "in_size", "out_offset", "out_size"] "success" (Scope
           [(Let "__" (Lit 1337133713371337)), (IfElse (Eq (Var "to") (Lit 0)) (Scope [M.boom]) (Scope []))
           ,(Assign "in_offset" (Add (Var "in_offset") (Lit memoryStashSize)))
           ,(Assign "out_offset" (Add (Var "out_offset") (Lit memoryStashSize)))
           ,(IfElse (Eq (Var "to") (ProcCall "mc" []))
                -- Internal call
                -- TODO: Deal with non-zero value calls
                (Scope [(Assign "to" Address)
                    --    ,(Let "_" (ProcCall "memcpy" [(Var "out_offset"), (Lit 2000), (Var "out_size")]))
                    --    ,(Assign "success" (Lit 1))
                       ,(Let "_" (ProcCall "memcpy" [(Lit 0), (Sub (Var "in_offset") (Lit 0x40)), (Lit 0x40)]))
                       ,(Mstore (Sub (Var "in_offset") (Lit 0x40)) (ProcCall "mc" []))
                       ,(Mstore (Sub (Var "in_offset") (Lit 0x20)) (Lit 0))
                       ,(Assign "success" (Call Gas (Var "to") (Lit 0) (Sub (Var "in_offset") (Lit 0x40)) (Add (Var "in_size") (Lit 0x40)) (Lit 0) (Lit 0)))
                       ,(Assign "_" (ProcCall "memcpy2" [(Sub (Var "in_offset") (Lit 0x40)), (Lit 0), (Lit 0x40)]))
                       ,(IfElse (Var "success")
                            (Scope [(Returndatacopy (Lit 0x0) (Lit 0x0) (Lit 0x20))
                                   ,(IfElse (Not (Mload (Lit 0x0)))
                                        (Scope [(Returndatacopy (Var "out_offset") (Lit 0x20) (ProcCall "min" [(Var "out_size"), (Sub Returndatasize (Lit 0x20))]))])
                                        (Scope [(Assign "success" (Lit 0))]))])
                            (Scope []))
                            ])
                (Scope [(IfElse (M.leq (Var "to") (Lit maxPrecompileAddress))
                       --,(IfElse (Lit 0)
                            -- Calls to precompiles are straight-forward
                            (Scope [(Assign "success" (Call (Var "gas") (Var "to") (Var "value") (Var "in_offset") (Var "in_size") (Var "out_offset") (Var "out_size")))])
                            -- External call: Backup 5 words following input, append extra info for MC to input, perform call, restore backup, access returndata
                            --(Scope [M.boom, (Assign "success" (Lit 1))]))
                            (Scope [(Let "in_end" (Add (Var "in_offset") (Var "in_size")))
                                   ,(Let "_" (ProcCall "memcpy" [(Lit 0), (Var "in_end"), (Lit (5 * 0x20))]))
                                   ,(Mstore (Add (Var "in_end") (Lit 0x00)) (Var "gas"))
                                   ,(Mstore (Add (Var "in_end") (Lit 0x20)) (Var "to"))
                                   ,(Mstore (Add (Var "in_end") (Lit 0x40)) (Var "value"))
                                   ,(Mstore (Add (Var "in_end") (Lit 0x60)) (Var "out_size"))
                                   ,(Mstore (Add (Var "in_end") (Lit 0x80)) (Lit 5) {- 5 is code for CALL-})
                                   ,(IfElse (Call Gas (ProcCall "mc" []) (Lit 0) (Var "in_offset") (Add (Lit (5 * 0x20)) (Var "in_size")) (Lit 0) (Lit 0))
                                        (Scope [(Assign "_" (ProcCall "memcpy2" [(Var "in_end"), (Lit 0), (Lit (5 * 0x20))]))
                                               ,(Returndatacopy (Lit 0) (Sub Returndatasize (Lit 0x20)) (Lit 0x20))
                                               ,(Assign "success" (Mload (Lit 0)))
                                               -- only write to output region if call succeeded
                                               ,(IfElse (Var "success")
                                                    (Scope [(Returndatacopy (Var "out_offset") (Lit 0) (Add (Var "out_size") (Lit 0)))])
                                                    (Scope []))])
                                        (Scope [(M.boom)]))
                                   ]))]))
           ])

procBalance = Proc "balance" ["address"] "balance" (Scope
              [(Mstore (Lit 0x00) (Var "address"))
              ,(Mstore (Lit 0x20) (Lit 6) {- 6 is code for BALANCE -})
              ,(IfElse (Call Gas (ProcCall "mc" []) (Lit 0) (Lit 0) (Lit 0x40) (Lit 0x0) (Lit 0x20))
                   (Scope [(Assign "balance" (Mload (Lit 0x0)))])
                   (Scope [(M.boom)]))
              ])

procReturn = Proc "return" ["offset", "size"] "_" (Scope
             [(Assign "offset" (Add (Var "offset") (Lit memoryStashSize)))
             ,(Mstore (Sub (Var "offset") (Lit 0x20)) (Var "size"))
             ,(Return (Sub (Var "offset") (Lit 0x20)) (Add (Lit 0x20) (Var "size")))
             ])

procInit = Proc "init" [] "_" (Scope
           [(IfElse (M.and3 (Or (Eq Caller (ProcCall "mc" [])) (Eq Caller Address)) (Iszero Callvalue) (Gt Calldatasize (Lit (0x40-1))))
                (Scope [(Mstore (Lit (memoryStashSize - 0x20)) (Lit 1))])
                (Scope [(M.boom)]))])

procDie = Proc "die" [] "_" (Scope
          [(Mstore (Lit 0) (Not (Lit 0)))
          ,(Return (Lit 0) (Lit 0x20))
          ])

procMc mc = Proc "mc" [] "address" (Scope [(Assign "address" (Lit mc))])

{-
procLog :: Procedure
procLog = Procedure "log" [V "num_topics", V "in offset", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"] (S [])
    [ TagJumpdest "log"             -- ⤳ S [Ret, V "num_topics", V "in offset", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    -- adjust offset by memoryStashSize
    , SWAP 2                        -- ⤳ S [V "in offset", V "num_topics", Ret, V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push memoryStashSize          -- ⤳ S [N "memoryStashSize", V "in offset", V "num_topics", Ret, V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ADD                           -- ⤳ S [N "in offset'", V "num_topics", Ret, V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , SWAP 2                        -- ⤳ S [Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    -- back up memory
    , DUP 2                         -- ⤳ S [V "num_topics", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push 1                        -- ⤳ S [N "1", V "num_topics",  Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ADD                           -- ⤳ S [N "num_topics+1", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push 0x20                     -- ⤳ S [N "0x20", V "num_topics+1", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , MUL                           -- ⤳ S [N "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push 0                        -- ⤳ S [N "0", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 1                         -- ⤳ S [V "0", V "0", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 3                         -- ⤳ S [V "(num_topics+1)*0x20", V "0", V "0", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Nop                           -- ⤳ S [V "(num_topics+1)*0x20", "0" `Rename` "excl_dst", "0" `Rename` "excl_dst_size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 8                         -- ⤳ S [V "in size", V "(num_topics+1)*0x20", V "excl_dst", V "excl_dst_size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 8                         -- ⤳ S [V "in offset'", V "in size", V "(num_topics+1)*0x20", V "excl_dst", V "excl_dst_size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ADD                           -- ⤳ S [N "in offset' + in size", V "(num_topics+1)*0x20", V "excl_dst", V "excl_dst_size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push 0                        -- ⤳ S [N "0", V "in offset' + in size", V "(num_topics+1)*0x20", V "excl_dst", V "excl_dst_size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Nop                           -- ⤳ S ["0" `Rename` "dst", "in offset' + in size" `Rename` "src", "(num_topics+1)*0x20" `Rename` "size", V "excl_dst", V "excl_dst_size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ProcedureCall "memcpy_excl"   -- ⤳ S [V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 5                         -- ⤳ S [V "in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 5                         -- ⤳ S [V "in offset'", V "in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ADD                           -- ⤳ S [N "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    -- first topic
    , DUP 4                         -- ⤳ S [V "num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push 1                        -- ⤳ S [N "1", V "num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , GT                            -- ⤳ S [N "1 > num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , TagJumpi "no_more_topics"     -- ⤳ S [V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 7                         -- ⤳ S [V "topic1", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 2                         -- ⤳ S [V "in offset' + in size", V "topic1", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , MSTORE                        -- ⤳ S [V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    -- second topic (30)
    , DUP 4                         -- ⤳ S [V "num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push 2                        -- ⤳ S [N "2", V "num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , GT                            -- ⤳ S [N "2 > num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , TagJumpi "no_more_topics"     -- ⤳ S [V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 8                         -- ⤳ S [V "topic2", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 2                         -- ⤳ S [V "in offset' + in size", V "topic2", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push (1 * 0x20)               -- ⤳ S [N "1 * 0x20", V "in offset' + in size", V "topic2", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ADD                           -- ⤳ S [N "in offset' + in size + 1 * 0x20", V "topic2", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , MSTORE                        -- ⤳ S [V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    -- third topic (39)
    , DUP 4                         -- ⤳ S [V "num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push 3                        -- ⤳ S [N "3", V "num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , GT                            -- ⤳ S [N "3 > num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , TagJumpi "no_more_topics"     -- ⤳ S [V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 9                         -- ⤳ S [V "topic3", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 2                         -- ⤳ S [V "in offset' + in size", V "topic3", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push (2 * 0x20)               -- ⤳ S [N "2 * 0x20", V "in offset' + in size", V "topic3", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ADD                           -- ⤳ S [N "in offset' + in size + 2 * 0x20", V "topic3", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , MSTORE                        -- ⤳ S [V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    -- fourth topic (48)
    , DUP 4                         -- ⤳ S [V "num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push 4                        -- ⤳ S [N "4", V "num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , GT                            -- ⤳ S [N "4 > num_topics", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , TagJumpi "no_more_topics"     -- ⤳ S [V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 10                        -- ⤳ S [V "topic4", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 2                         -- ⤳ S [V "in offset' + in size", V "topic4", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push (3 * 0x20)               -- ⤳ S [N "3 * 0x20", V "in offset' + in size", V "topic4", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ADD                           -- ⤳ S [N "in offset' + in size + 3 * 0x20", V "topic4", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , MSTORE                        -- ⤳ S [V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    -- stored all topics in memory
    , TagJumpdest "no_more_topics"  -- ⤳ S [V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 2                         -- ⤳ S [V "(num_topics+1)*0x20", V "in offset' + in size", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , SWAP 1                        -- ⤳ S [V "in offset' + in size", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    -- store number of topics in memory
    , DUP 2                         -- ⤳ S [V "(num_topics+1)*0x20", V "in offset' + in size", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ADD                           -- ⤳ S [N "in offset' + in size + (num_topics+1)*0x20", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push 0x20                     -- ⤳ S [N "0x20", V "in offset' + in size + (num_topics+1)*0x20", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , SWAP 1                        -- ⤳ S [V "in offset' + in size + (num_topics+1)*0x20", V "0x20", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , SUB                           -- ⤳ S [N "in offset' + in size + num_topics*0x20", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 5                         -- ⤳ S [V "num_topics", V "in offset' + in size + num_topics*0x20", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , SWAP 1                        -- ⤳ S [V "in offset' + in size + num_topics*0x20", V "num_topics", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , MSTORE                        -- ⤳ S [V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    -- CALL
    , DUP 6                         -- ⤳ S [V "in size", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ADD                           -- ⤳ S [N "in size + (num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 1                         -- ⤳ S [V "in size + (num_topics+1)*0x20", V "in size + (num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 1                         -- ⤳ S [V "in size + (num_topics+1)*0x20", V "in size + (num_topics+1)*0x20", V "in size + (num_topics+1)*0x20", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Nop                           -- ⤳ S [V "in size + (num_topics+1)*0x20", "in size + (num_topics+1)*0x20" `Rename` "empty output", "in size + (num_topics+1)*0x20" `Rename` "empty output", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 7                         -- ⤳ S [V "in offset'", V "in size + (num_topics+1)*0x20", V "empty output", V "empty output", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push 0                        -- ⤳ S [N "0", V "in offset'", V "in size + (num_topics+1)*0x20", V "empty output", V "empty output", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ProcedureCall "mc_address"    -- ⤳ S [N "mc_address", V "0", V "in offset'", V "in size + (num_topics+1)*0x20", V "empty output", V "empty output", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , GAS                           -- ⤳ S [N "gas", V "mc_address", V "0", V "in offset'", V "in size + (num_topics+1)*0x20", V "empty output", V "empty output", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , CALL                          -- ⤳ S [N "success", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ISZERO                        -- ⤳ S [N "fail", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , PC                            -- ⤳ S [N "invalid jump destination", V "fail", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , JUMPI                         -- ⤳ S [V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    -- restore memory from backup
    , Push 0                        -- ⤳ S [N "0", V "(num_topics+1)*0x20", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , SWAP 1                        -- ⤳ S [V "(num_topics+1)*0x20", V "0", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 1                         -- ⤳ S [V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", V "0", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 3                         -- ⤳ S [V "0", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", V "0", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 8                         -- ⤳ S [V "in size", V "0", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", V "0", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , DUP 8                         -- ⤳ S [V "in offset'", V "in size", V "0", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", V "0", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ADD                           -- ⤳ S [N "in offset' + in size", V "0", V "(num_topics+1)*0x20", V "(num_topics+1)*0x20", V "0", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , Nop                           -- ⤳ S ["in offset' + in size" `Rename` "dst", "0" `Rename` "src", "(num_topics+1)*0x20" `Rename` "size", "(num_topics+1)*0x20" `Rename` "excl_dst", "0" `Rename` "excl_dst_size", Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    , ProcedureCall "memcpy_excl"   -- ⤳ S [Ret, V "num_topics", V "in offset'", V "in size", V "topic1", V "topic2", V "topic3", V "topic4"]
    -- clean stack
    , SWAP 3                        -- ⤳ S [V "in size", V "num_topics", V "in offset'", Ret, V "topic1", V "topic2", V "topic3", V "topic4"]
    , POP                           -- ⤳ S [V "num_topics", V "in offset'", Ret, V "topic1", V "topic2", V "topic3", V "topic4"]
    , SWAP 1                        -- ⤳ S [V "in offset'", V "num_topics", Ret, V "topic1", V "topic2", V "topic3", V "topic4"]
    , POP                           -- ⤳ S [V "num_topics", Ret, V "topic1", V "topic2", V "topic3", V "topic4"]
    -- first item
    , DUP 1                         -- ⤳ S [V "num_topics", V "num_topics", Ret, V "topic1", V "topic2", V "topic3", V "topic4"]
    , Push 1                        -- ⤳ S [N "1", V "num_topics", V "num_topics", Ret, V "topic1", V "topic2", V "topic3", V "topic4"]
    , GT                            -- ⤳ S [N "1 > num_topics", V "num_topics", Ret, V "topic1", V "topic2", V "topic3", V "topic4"]
    , StateForce                    -- ⤳ S [N "1 > num_topics", V "num_topics", Ret]
    , TagJumpi "no_more_topics2"    -- ⤳ S [V "num_topics", Ret]
    , StateForce                    -- ⤳ S [V "num_topics", Ret, V "topic1", V "topic2", V "topic3", V "topic4"]
    , SWAP 2                        -- ⤳ S [V "topic1", Ret, V "num_topics", V "topic2", V "topic3", V "topic4"]
    , POP                           -- ⤳ S [Ret, V "num_topics", V "topic2", V "topic3", V "topic4"]
    , SWAP 1                        -- ⤳ S [V "num_topics", Ret, V "topic2", V "topic3", V "topic4"]
    -- second item
    , DUP 1                         -- ⤳ S [V "num_topics", V "num_topics", Ret, V "topic2", V "topic3", V "topic4"]
    , Push 2                        -- ⤳ S [N "2", V "num_topics", V "num_topics", Ret, V "topic2", V "topic3", V "topic4"]
    , GT                            -- ⤳ S [N "2 > num_topics", V "num_topics", Ret, V "topic2", V "topic3", V "topic4"]
    , StateForce                    -- ⤳ S [N "2 > num_topics", V "num_topics", Ret]
    , TagJumpi "no_more_topics2"    -- ⤳ S [V "num_topics", Ret]
    , StateForce                    -- ⤳ S [V "num_topics", Ret, V "topic2", V "topic3", V "topic4"]
    , SWAP 2                        -- ⤳ S [V "topic2", Ret, V "num_topics", V "topic3", V "topic4"]
    , POP                           -- ⤳ S [Ret, V "num_topics", V "topic3", V "topic4"]
    , SWAP 1                        -- ⤳ S [V "num_topics", Ret, V "topic3", V "topic4"]
    -- third item
    , DUP 1                         -- ⤳ S [V "num_topics", V "num_topics", Ret, V "topic3", V "topic4"]
    , Push 3                        -- ⤳ S [N "3", V "num_topics", V "num_topics", Ret, V "topic3", V "topic4"]
    , GT                            -- ⤳ S [N "3 > num_topics", V "num_topics", Ret, V "topic3", V "topic4"]
    , StateForce                    -- ⤳ S [N "3 > num_topics", V "num_topics", Ret]
    , TagJumpi "no_more_topics2"    -- ⤳ S [V "num_topics", Ret]
    , StateForce                    -- ⤳ S [V "num_topics", Ret, V "topic3", V "topic4"]
    , SWAP 2                        -- ⤳ S [V "topic3", Ret, V "num_topics", V "topic4"]
    , POP                           -- ⤳ S [Ret, V "num_topics", V "topic4"]
    , SWAP 1                        -- ⤳ S [V "num_topics", Ret, V "topic4"]
    -- fourth item
    , DUP 1                         -- ⤳ S [V "num_topics", V "num_topics", Ret, V "topic4"]
    , Push 4                        -- ⤳ S [N "4", V "num_topics", V "num_topics", Ret, V "topic4"]
    , GT                            -- ⤳ S [N "4 > num_topics", V "num_topics", Ret, V "topic4"]
    , StateForce                    -- ⤳ S [N "4 > num_topics", V "num_topics", Ret]
    , TagJumpi "no_more_topics2"    -- ⤳ S [V "num_topics", Ret]
    , StateForce                    -- ⤳ S [V "num_topics", Ret, V "topic4"]
    , SWAP 2                        -- ⤳ S [V "topic4", Ret, V "num_topics"]
    , POP                           -- ⤳ S [Ret, V "num_topics"]
    , SWAP 1                        -- ⤳ S [V "num_topics", Ret]
    -- stack cleaned
    , TagJumpdest "no_more_topics2" -- ⤳ S [V "num_topics", Ret]
    , POP                           -- ⤳ S [Ret]
    , ProcedureReturn               -- ⤳ Ø
    ]

-- calling convention stuff: when calling, prepend calldata with own (not MC's!) address and callvalue
--                           upon return, skip first word of return data. If it returns 0xffff..ffff, also return 0xfff.fff
--                           Otherwise, skip first word, treat rest as output data. If call fails, also fail call.

check :: a -> Bool -> a
check x b = if b then x else error "check failure"

procCall :: Procedure
procCall = Procedure "call" [V "gas", V "to", V "value", V "in offset", V "in size", V "out offset", V "out size"] (S [V "success"]) $
    [ TagJumpdest "call"          -- ⤳ S [Ret, V "gas", V "to", V "value", V "in offset", V "in size", V "out offset", V "out size"]
    -- Is this a call to self?
    -- TODO(lorenzb): This might be a good place to check that value doesn't exceed mc.balance. Ask Florian what he thinks
    , ProcedureCall "mc_address"  -- ⤳ S [N "mc_address", Ret, V "gas", V "to", V "value", V "in offset", V "in size", V "out offset", V "out size"]
    , DUP 4                       -- ⤳ S [V "to", V "mc_address", Ret, V "gas", V "to", V "value", V "in offset", V "in size", V "out offset", V "out size"]
    , EQ                          -- ⤳ S [N "to == mc_address", Ret, V "gas", V "to", V "value", V "in offset", V "in size", V "out offset", V "out size"]
    , TagJumpi "internal_call"    -- ⤳ S [Ret, V "gas", V "to", V "value", V "in offset", V "in size", V "out offset", V "out size"]
    -- Adjust offsets by memoryStashSize
    , SWAP 4                      -- ⤳ S [V "in offset", V "gas", V "to", V "value", Ret, V "in size", V "out offset", V "out size"]
    , Push memoryStashSize        -- ⤳ S [N "memory stash size", V "in offset", V "gas", V "to", V "value", Ret, V "in size", V "out offset", V "out size"]
    , ADD                         -- ⤳ S [N "in offset'", V "gas", V "to", V "value", Ret, V "in size", V "out offset", V "out size"]
    , SWAP 4                      -- ⤳ S [Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset", V "out size"]
    , SWAP 6                      -- ⤳ S [V "out offset", V "gas", V "to", V "value", V "in offset'", V "in size", Ret, V "out size"]
    , Push memoryStashSize        -- ⤳ S [N "memory stash size", V "out offset", V "gas", V "to", V "value", V "in offset'", V "in size", Ret, V "out size"]
    , ADD                         -- ⤳ S [N "out offset'", V "gas", V "to", V "value", V "in offset'", V "in size", Ret, V "out size"]
    , SWAP 6                      -- ⤳ S [Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    -- If this is a call to a precompile, that's all we need to do. (assuming that precompiles don't accept value != 0. TODO(lorenzb): Verify this assumption)
    , Push maxPrecompileAddress   -- ⤳ S [N "maxPrecompileAddress", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 4                       -- ⤳ S [V "to", V "maxPrecompileAddress", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , GT                          -- ⤳ S [N "not precompile", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , TagJumpi "regular_call"     -- ⤳ S [Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0                      -- ⤳ S [N "0", Ret    , V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , StateForce                  -- ⤳ S [V "0", V "Ret", V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , MSTORE                      -- ⤳ S [V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , CALL                        -- ⤳ S [N "success"]
    , Push 0                      -- ⤳ S [N "0", V "success"]
    , MLOAD                       -- ⤳ S [N "Ret", V "success"]
    , StateForce                  -- ⤳ S [Ret    , V "success"]
    , ProcedureReturn             -- ⤳ Ø
    , TagJumpdest "regular_call"  -- ⤳ S [Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    -- Backup parts of output region overwritten by instrumentation to M[0..31]
    , DUP 8                       -- ⤳ S [V "out size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 8                       -- ⤳ S [V "out offset'", V "out size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , ADD                         -- ⤳ S [N "out offset' + out size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , MLOAD                       -- ⤳ S [N "M[out offset' + out size]", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0                      -- ⤳ S [N "0", V "M[out offset' + out size]", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , MSTORE                      -- ⤳ S [Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    -- Backup parts of input region overwritten by instrumentation to M[32..191]
    , Push 0                      -- ⤳ S [N "0", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 1                       -- ⤳ S [V "0", V "0", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push (5 * 0x20)             -- ⤳ S [N "5*0x20", V "0", V "0", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 9                       -- ⤳ S [V "in size", V "5*0x20", V "0", V "0", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 9                       -- ⤳ S [V "in offset'", V "in size", V "5*0x20", V "0", V "0", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , ADD                         -- ⤳ S [N "in offset' + in size", V "5*0x20", V "0", V "0", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0x20                   -- ⤳ S [N "0x20", V "in offset' + in size", V "5*0x20", V "0", V "0", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , Nop                         -- ⤳ S ["0x20" `Rename` "dst", "in offset' + in size" `Rename` "src", "5*0x20" `Rename` "size", "0" `Rename` "excl_dst", "0" `Rename` "excl_dst_size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , ProcedureCall "memcpy_excl" -- ⤳ S [Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    -- Append gas, to, value, out size, 5 to input
    , DUP 6                       -- ⤳ S [V "in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 6                       -- ⤳ S [V "in offset'", V "in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , ADD                         -- ⤳ S [N "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    --   append gas
    , DUP 3                       -- ⤳ S [V "gas", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 2                       -- ⤳ S [V "in offset' + in size", V "gas", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , MSTORE                      -- ⤳ S [V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    --   append to
    , DUP 4                       -- ⤳ S [V "to", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 2                       -- ⤳ S [V "in offset' + in size", V "to", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0x20                   -- ⤳ S [N "0x20", V "in offset' + in size", V "to", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , ADD                         -- ⤳ S [N "in offset' + in size + 0x20", V "to", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , MSTORE                      -- ⤳ S [V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    --   append value
    , DUP 5                       -- ⤳ S [V "value", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 2                       -- ⤳ S [V "in offset' + in size", V "value", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0x40                   -- ⤳ S [N "0x40", V "in offset' + in size", V "value", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , ADD                         -- ⤳ S [N "in offset' + in size + 0x40", V "value", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , MSTORE                      -- ⤳ S [V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    --   append out size
    , DUP 9                       -- ⤳ S [V "out size", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 2                       -- ⤳ S [V "in offset' + in size", V "out size", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0x60                   -- ⤳ S [N "0x60", V "in offset' + in size", V "out size", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , ADD                         -- ⤳ S [N "in offset' + in size + 0x60", V "out size", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , MSTORE                      -- ⤳ S [V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    --   append 5
    , Push 5                      -- ⤳ S [N "5", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 2                       -- ⤳ S [V "in offset' + in size", V "5", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0x80                   -- ⤳ S [N "0x80", V "in offset' + in size", V "5", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , ADD                         -- ⤳ S [N "in offset' + in size + 0x80", V "5", V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , MSTORE                      -- ⤳ S [V "in offset' + in size", Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    -- Clean up stack a little
    , POP                         -- ⤳ S [Ret, V "gas", V "to", V "value", V "in offset'", V "in size", V "out offset'", V "out size"]
    , SWAP 3                      -- ⤳ S [V "value", V "gas", V "to", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , POP                         -- ⤳ S [V "gas", V "to", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , POP                         -- ⤳ S [V "to", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , POP                         -- ⤳ S [Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    -- Prepare arguments for CALL, perform CALL, crash on failure
    --   Add 0x20 to outsize (for success)
    , DUP 5                       -- ⤳ S [V "out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0x20                   -- ⤳ S [N "0x20", V "out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , ADD                         -- ⤳ S [N "out size + 0x20", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    --   out offset'
    , DUP 5                       -- ⤳ S [V "out offset'", V "out size + 0x20", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    --   in size + 0xa0 (for gas, to, value, out size, 5)
    , DUP 5                       -- ⤳ S [V "in size", V "out offset'", V "out size + 0x20", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push (5 * 0x20)             -- ⤳ S [N "0xA0", V "in size", V "out offset'", V "out size + 0x20", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , ADD                         -- ⤳ S [N "in size + 0xA0", V "out offset'", V "out size + 0x20", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    --   in offset
    , DUP 5                       -- ⤳ S [V "in offset'", V "in size + 0xA0", V "out offset'", V "out size + 0x20", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    --   value = 0
    , Push 0                      -- ⤳ S [N "0", V "in offset'", V "in size + 0xA0", V "out offset'", V "out size + 0x20", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    --   to = mc
    , ProcedureCall "mc_address"  -- ⤳ S [N "mc_address", V "0", V "in offset'", V "in size + 0xA0", V "out offset'", V "out size + 0x20", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    --   gas = all remaining gas
    , GAS                         -- ⤳ S [N "gas", V "mc_address", V "0", V "in offset'", V "in size + 0xA0", V "out offset'", V "out size + 0x20", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    --    CALL
    , CALL                        -- ⤳ S [N "call succeeded", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    --    crash on failure
    , ISZERO                      -- ⤳ S [N "call failed", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , PC                          -- ⤳ S [N "invalid jump destination", V "call failed", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , JUMPI                       -- ⤳ S [Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    -- Restore output backup and store return value in M[0]
    , DUP 5                       -- ⤳ S [V "out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 5                       -- ⤳ S [V "out offset'", V "out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , ADD                         -- ⤳ S [N "out offset' + out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 1                       -- ⤳ S [V "out offset' + out size", V "out offset' + out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , MLOAD                       -- ⤳ S [N "return value", V "out offset' + out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0                      -- ⤳ S [N "0", V "return value", V "out offset' + out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , MLOAD                       -- ⤳ S [N "M[0]", V "return value", V "out offset' + out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , SWAP 1                      -- ⤳ S [V "return value", V "M[0]", V "out offset' + out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0                      -- ⤳ S [N "0", V "return value", V "M[0]", V "out offset' + out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , MSTORE                      -- ⤳ S [V "M[0]", V "out offset' + out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , SWAP 1                      -- ⤳ S [V "out offset' + out size", V "M[0]", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , MSTORE                      -- ⤳ S [Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    -- Restore input backup whereever it does not overlap with output
    , DUP 5                       -- ⤳ S [V "out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 5                       -- ⤳ S [V "out offset'", V "out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0xA0                   -- ⤳ S [N "0xA0", V "out offset'", V "out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , Push 0x20                   -- ⤳ S [N "0x20", V "0xA0", V "out offset'", V "out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 7                       -- ⤳ S [V "in size", V "0x20", V "0xA0", V "out offset'", V "out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , DUP 7                       -- ⤳ S [V "in offset'", V "in size", V "0x20", V "0xA0", V "out offset'", V "out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , ADD                         -- ⤳ S [N "in offset' + in size", V "0x20", V "0xA0", V "out offset'", V "out size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , Nop                         -- ⤳ S ["in offset' + in size" `Rename` "dst", "0x20" `Rename` "src", "0xA0" `Rename` "size", "out offset'" `Rename` "excl_dst", "out size" `Rename` "excl_dst_size", Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    , ProcedureCall "memcpy_excl" -- ⤳ S [Ret, V "in offset'", V "in size", V "out offset'", V "out size"]
    -- Clean up stack
    , SWAP 4                      -- ⤳ S [V "out size", V "in offset'", V "in size", V "out offset'", Ret]
    , POP                         -- ⤳ S [V "in offset'", V "in size", V "out offset'", Ret]
    , POP                         -- ⤳ S [V "in size", V "out offset'", Ret]
    , POP                         -- ⤳ S [V "out offset'", Ret]
    , POP                         -- ⤳ S [Ret]
    -- Load success from M[0..31] and return
    , Push 0                      -- ⤳ S [N "0", Ret]
    , MLOAD                       -- ⤳ S [N "success", Ret]
    , SWAP 1                      -- ⤳ S [Ret, V "success"]
    , ProcedureReturn             -- ⤳ Ø
    ] ++
    let stashRet          = 0 * 0x20
        stashOutputPrefix = 1 * 0x20
        stashInputPrefix1 = 2 * 0x20
        stashInputPrefix2 = 3 * 0x20
        stashInputOffset  = 4 * 0x20
        stashOutputOffset = 5 * 0x20
        stashOutputSize   = 6 * 0x20
        inputPrefixWidth  = 2 * 0x20
        outputPrefixWidth = 1 * 0x20
        -- since we prefix memory (with at most two words), this procedure requires at memoryStashSize >= (6 + 1 + 2) * 0x20
    in
    -----------------
    -- Internal call
    -----------------
    [ TagJumpdest "internal_call"                           -- ⤳ S [Ret, V "gas", V "to", V "value", V "in offset", V "in size", V "out offset", V "out size"]
    -- We don't call any other procedures that modify M[0] in this procedure, so we can safely store the return pc there
    , StateForce                                            -- ⤳ S [N "Ret", V "gas", V "to", V "value", V "in offset", V "in size", V "out offset", V "out size"]
    , Push 0                                                -- ⤳ S [N "0", V "Ret", V "gas", V "to", V "value", V "in offset", V "in size", V "out offset", V "out size"]
    , MSTORE                                                -- ⤳ S [V "gas", V "to", V "value", V "in offset", V "in size", V "out offset", V "out size"] -- TODO(lorenzb): Should we give internal calls control over gas?
    -- Adjust input and output regions
    , SWAP 3                                                -- ⤳ S [V "in offset", V "to", V "value", V "gas", V "in size", V "out offset", V "out size"]
    , Push (memoryStashSize - inputPrefixWidth)             -- ⤳ S [N "memoryStashSize - in pre width", V "in offset", V "to", V "value", V "gas", V "in size", V "out offset", V "out size"]
    , ADD                                                   -- ⤳ S [N "in offset' - in pre width", V "to", V "value", V "gas", V "in size", V "out offset", V "out size"]
    , SWAP 3                                                -- ⤳ S [V "gas", V "to", V "value", V "in offset' - in pre width", V "in size", V "out offset", V "out size"]
    , SWAP 4                                                -- ⤳ S [V "in size", V "to", V "value", V "in offset' - in pre width", V "gas", V "out offset", V "out size"]
    , Push inputPrefixWidth                                 -- ⤳ S [N "in pre width", V "in size", V "to", V "value", V "in offset' - in pre width", V "gas", V "out offset", V "out size"]
    , ADD                                                   -- ⤳ S [N "in size + in pre width", V "to", V "value", V "in offset' - in pre width", V "gas", V "out offset", V "out size"]
    , SWAP 4                                                -- ⤳ S [V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset", V "out size"]
    , SWAP 5                                                -- ⤳ S [V "out offset", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "gas", V "out size"]
    , Push (memoryStashSize - outputPrefixWidth)            -- ⤳ S [N "memoryStashSize - out pre width", V "out offset", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "gas", V "out size"]
    , ADD                                                   -- ⤳ S [N "out offset' - out pre width", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "gas", V "out size"]
    , SWAP 5                                                -- ⤳ S [V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size"]
    , SWAP 6                                                -- ⤳ S [V "out size", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "gas"]
    , Push outputPrefixWidth                                -- ⤳ S [N "out pre width", V "out size", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "gas"]
    , ADD                                                   -- ⤳ S [N "out size + out pre width", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "gas"]
    , SWAP 6                                                -- ⤳ S [V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    -- Backup prefix of output region (1 word)
    , DUP 6                                                 -- ⤳ S [V "out offset' - out pre width", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , MLOAD                                                 -- ⤳ S [N "M[out offset' - out pre width]", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , Push stashOutputPrefix                                -- ⤳ S [N "stashOutputPrefix", V "M[out offset' - out pre width]", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , MSTORE                                                -- ⤳ S [V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    -- Backup prefix of input region (2 words)
    , DUP 4                                                 -- ⤳ S [V "in offset' - in pre width", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , MLOAD                                                 -- ⤳ S [N "M[in offset' - in pre width]", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , Push stashInputPrefix1                                -- ⤳ S [N "stashInputPrefix1", V "M[in offset' - in pre width]", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , MSTORE                                                -- ⤳ S [V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , DUP 4                                                 -- ⤳ S [V "in offset' - in pre width", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , Push 0x20                                             -- ⤳ S [N "0x20", V "in offset' - in pre width", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , ADD                                                   -- ⤳ S [N "in offset' - in pre width + 0x20", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , MLOAD                                                 -- ⤳ S [N "M[in offset' - in pre width + 0x20]", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , Push stashInputPrefix2                                -- ⤳ S [N "stashInputPrefix2", V "M[in offset' - in pre width + 0x20]", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , MSTORE                                                -- ⤳ S [V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    -- Backup "in offset' - in pre width"
    , DUP 4                                                 -- ⤳ S [V "in offset' - in pre width", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , Push stashInputOffset                                 -- ⤳ S [N "stashInputOffset", V "in offset' - in pre width", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , MSTORE                                                -- ⤳ S [V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    -- Backup "out offset' - out pre width" to M[160..191]
    , DUP 6                                                 -- ⤳ S [V "out offset' - out pre width", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , Push stashOutputOffset                                -- ⤳ S [N "stashOutputOffset", V "out offset' - out pre width", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , MSTORE                                                -- ⤳ S [V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    -- Backup "out size + out pre width" to M[192..223]
    , DUP 7                                                 -- ⤳ S [V "out size + out pre width", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , Push stashOutputSize                                  -- ⤳ S [N "stashOutputSize", V "out size + out pre width", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , MSTORE                                                -- ⤳ S [V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    -- Store address for call data
    , ProcedureCall "mc_address"                            -- ⤳ S [N "mc_address", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , DUP 5                                                 -- ⤳ S [V "in offset' - in pre width", V "mc_address", V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , MSTORE                                                -- ⤳ S [V "gas", V "to", V "value", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    -- Check, store, and change call value
    , SWAP 2                                                -- ⤳ S [V "value", V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , DUP 1                                                 -- ⤳ S [V "value", V "value", V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , ProcedureCall "mc_address"                            -- ⤳ S [N "mc_address", V "value", V "value", V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , BALANCE                                               -- ⤳ S [N "balance", V "value", V "value", V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , LT                                                    -- ⤳ S [N "balance < value", V "value", V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , PC                                                    -- ⤳ S [N "invalid jump destination", V "balance < value", V "value", V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , JUMPI                                                 -- ⤳ S [V "value", V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , DUP 4                                                 -- ⤳ S [V "in offset' - in pre width", V "value", V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , Push 0x20                                             -- ⤳ S [N "0x20", V "in offset' - in pre width", V "value", V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , ADD                                                   -- ⤳ S [N "in offset' - in pre width + 0x20", V "value", V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , MSTORE                                                -- ⤳ S [V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , Push 0                                                -- ⤳ S [N "0", V "to", V "gas", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , SWAP 2                                                -- ⤳ S [V "gas", V "to", V "0", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    -- Replace to with own address
    , SWAP 1                                                -- ⤳ S [V "to", V "gas", V "0", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , POP                                                   -- ⤳ S [V "gas", V "0", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , ADDRESS                                               -- ⤳ S [N "self address", V "gas", V "0", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    , SWAP 1                                                -- ⤳ S [V "gas", V "self address", V "0", V "in offset' - in pre width", V "in size + in pre width", V "out offset' - out pre width", V "out size + out pre width"]
    -- CALL
    , CALL                                                  -- ⤳ S [N "success"]
    , DUP 1                                                 -- ⤳ S [V "success", V "success"]
    , ISZERO                                                -- ⤳ S [N "fail", V "success"]
    , TagJumpi "skip_check_first_word_of_output"            -- ⤳ S [V "success"]
    -- Check whether first word of output indicates a throw and restore memory from backup
    , Push stashOutputOffset                                -- ⤳ S [N "stashOutputOffset", V "success"]
    , MLOAD                                                 -- ⤳ S [N "out offset' - out pre width", V "success"]
    , MLOAD                                                 -- ⤳ S [N "M[out offset' - out pre width]", V "success"]
    , NOT                                                   -- ⤳ S [N "M[out offset' - out pre width] != 0xff..ff", V "success"]
    , TagJumpi "dont_die"                                   -- ⤳ S [V "success"]
    , ProcedureCall "die"                                   -- ⤳ Ø
    , TagJumpdest "dont_die"                                -- ⤳ S [V "success"]
    , Push stashOutputPrefix                                -- ⤳ S [N "stashOutputPrefix", V "success"]
    , MLOAD                                                 -- ⤳ S [N "old M[out offset' - out pre width]", V "success"]
    , Push stashOutputOffset                                -- ⤳ S [N "stashOutputOffset", V "old M[out offset' - out pre width]", V "success"]
    , MLOAD                                                 -- ⤳ S [N "out offset' - out pre width", V "old M[out offset' - out pre width]", V "success"]
    , MSTORE                                                -- ⤳ S [V "success"]
    , TagJumpdest "skip_check_first_word_of_output"         -- ⤳ S [V "success"]
    -- Restore input region
    -- If the CALL failed, we want to restore all memory in the input region, even if it overlapped with the output
    , Push stashOutputSize                                  -- ⤳ S [N "stashOutputSize", V "success"]
    , MLOAD                                                 -- ⤳ S [N "out size + out pre width", V "success"]
    , DUP 2                                                 -- ⤳ S [V "success", V "out size + out pre width", V "success"]
    , MUL                                                   -- ⤳ S [N "success * (out size + out pre width)", V "success"]
    , Push stashOutputOffset                                -- ⤳ S [N "stashOutputOffset", V "success * (out size + out pre width)", V "success"]
    , MLOAD                                                 -- ⤳ S [N "out offset' - out pre width", V "success * (out size + out pre width)", V "success"]
    , Push inputPrefixWidth                                 -- ⤳ S [N "in pre width", V "out offset' - out pre width", V "success * (out size + out pre width)", V "success"]
    , DUP 1 `check` (stashInputPrefix1 == inputPrefixWidth) -- ⤳ S [V "in pre width", V "in pre width", V "out offset' - out pre width", V "success * (out size + out pre width)", V "success"]
    , Nop                                                   -- ⤳ S ["in pre width" `Rename` "stashInputPrefix1", V "in pre width", V "out offset' - out pre width", V "success * (out size + out pre width)", V "success"]
    , Push stashInputOffset                                 -- ⤳ S [N "stashInputOffset", V "stashInputPrefix1", V "in pre width", V "out offset' - out pre width", V "success * (out size + out pre width)", V "success"]
    , MLOAD                                                 -- ⤳ S [N "in offset' - in pre width", V "stashInputPrefix1",  V "in pre width", V "out offset' - out pre width", V "success * (out size + out pre width)", V "success"]
    , Nop                                                   -- ⤳ S ["in offset' - in pre width" `Rename` "dst", "stashInputPrefix1" `Rename` "src", "in pre width" `Rename` "size", "out offset' - out pre width" `Rename` "excl_dst", "success * (out size + out pre width)" `Rename` "excl_dst_size", V "success"]
    , ProcedureCall "memcpy_excl"                           -- ⤳ S [V "success"]

    -- Done, retrieve return pc from M[0] and return
    , Push 0                                                -- ⤳ S [N "0", V "success"]
    , MLOAD                                                 -- ⤳ S [N "Ret", V "success"]
    , StateForce                                            -- ⤳ S [Ret, V "success"]
    , ProcedureReturn                                       -- ⤳ Ø
    ]

procBalance = Procedure "balance" [V "address"] (S [V "balance"])
    [ TagJumpdest "balance"       -- ⤳ S [Ret, V "address"]
    , SWAP 1                      -- ⤳ S [V "address", Ret]
    , Push 0                      -- ⤳ S [N "0", V "address", Ret]
    , MSTORE                      -- ⤳ S [Ret]
    , Push 6                      -- ⤳ S [N "6", Ret]
    , Push 0x20                   -- ⤳ S [N "0x20", V "6", Ret]
    , MSTORE                      -- ⤳ S [Ret]
    , Push 0x20                   -- ⤳ S [N "out size = 0x20", Ret]
    , Push 0                      -- ⤳ S [N "out offset = 0", V "out size = 0x20", Ret]
    , Push 0x40                   -- ⤳ S [N "in size = 0x40", V "out offset = 0", V "out size = 0x20", Ret]
    , Push 0                      -- ⤳ S [N "in offset = 0", V "in size = 0x40", V "out offset = 0", V "out size = 0x20", Ret]
    , Push 0                      -- ⤳ S [N "value = 0", V "in offset = 0", V "in size = 0x40", V "out offset = 0", V "out size = 0x20", Ret]
    , ProcedureCall "mc_address"  -- ⤳ S [N "mc_address", V "value = 0", V "in offset = 0", V "in size = 0x40", V "out offset = 0", V "out size = 0x20", Ret]
    , GAS                         -- ⤳ S [N "gas", V "mc_address", V "value = 0", V "in offset = 0", V "in size = 0x40", V "out offset = 0", V "out size = 0x20", Ret]
    , CALL                        -- ⤳ S [N "success", Ret]
    , ISZERO                      -- ⤳ S [N "fail", Ret]
    , PC                          -- ⤳ S [N "pc", V "fail", Ret]
    , JUMPI                       -- ⤳ S [Ret]
    , Push 0                      -- ⤳ S [N "0", Ret]
    , MLOAD                       -- ⤳ S [N "balance", Ret]
    , SWAP 1                      -- ⤳ S [Ret, V "balance"]
    , ProcedureReturn             -- ⤳ Ø
    ]

procMemcpyExcl = Procedure "memcpy_excl" [V "dst", V "src", V "size", V "excl_dst", V "excl_dst_size"] (S [])
    [ TagJumpdest "memcpy_excl"           -- ⤳ S [Ret, V "dst", V "src", V "size", V "excl_dst", V "excl_dst_size"]
    , SWAP 3                              -- ⤳ S [V "size",  V "dst", V "src", Ret, V "excl_dst", V "excl_dst_size"]
    , TagJumpdest "memcpy_excl_loop"      -- ⤳ S [V "size",  V "dst", V "src", Ret, V "excl_dst", V "excl_dst_size"]
    , DUP 1                               -- ⤳ S [V "size", V "size",  V "dst", V "src", Ret, V "excl_dst", V "excl_dst_size"]
    , ISZERO                              -- ⤳ S [N "size == 0", V "size",  V "dst", V "src", Ret, V "excl_dst", V "excl_dst_size"]
    , TagJumpi "memcpy_excl_end"          -- ⤳ S [V "size",  V "dst", V "src", Ret, V "excl_dst", V "excl_dst_size"]
    , Push 1                              -- ⤳ S [N "1", V "size",  V "dst", V "src", Ret, V "excl_dst", V "excl_dst_size"]
    , SWAP 1                              -- ⤳ S [V "size", V "1", V "dst", V "src", Ret, V "excl_dst", V "excl_dst_size"]
    , SUB                                 -- ⤳ S [N "size - 1", V "dst", V "src", Ret, V "excl_dst", V "excl_dst_size"]
    , SWAP 2                              -- ⤳ S [V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , DUP 6                               -- ⤳ S [V "excl_dst_size", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , DUP 6                               -- ⤳ S [V "excl_dst", V "excl_dst_size", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , ADD                                 -- ⤳ S [N "excl_dst + excl_dst_size", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , DUP 3                               -- ⤳ S [V "dst", V "excl_dst + excl_dst_size", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , LT                                  -- ⤳ S [N "dst < excl_dst + excl_dst_size", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , DUP 6                               -- ⤳ S [V "excl_dst", V "dst < excl_dst + excl_dst_size", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , DUP 4                               -- ⤳ S [V "dst", V "excl_dst", V "dst < excl_dst + excl_dst_size", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , LT                                  -- ⤳ S [N "dst < excl_dst", V "dst < excl_dst + excl_dst_size", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , ISZERO                              -- ⤳ S [N "dst >= excl_dst", V "dst < excl_dst + excl_dst_size", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , AND                                 -- ⤳ S [N "dst >= excl_dst & dst < excl_dst + excl_dst_size", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , TagJumpi "memcpy_excl_skip_copy"    -- ⤳ S [V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , DUP 1                               -- ⤳ S [V "src", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , MLOAD                               -- ⤳ S [N "M[src:src+31]", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , Push 0                              -- ⤳ S [N "0", V "M[src:src+31]", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , BYTE                                -- ⤳ S [N "M[src]", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , DUP 3                               -- ⤳ S [V "dst", V "M[src]", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , MSTORE8                             -- ⤳ S [V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , TagJumpdest "memcpy_excl_skip_copy" -- ⤳ S [V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , Push 1                              -- ⤳ S [N "1", V "src", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , ADD                                 -- ⤳ S [N "src + 1", V "dst", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , SWAP 1                              -- ⤳ S [V "dst", V "src + 1", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , Push 1                              -- ⤳ S [N "1", V "dst", V "src + 1", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , ADD                                 -- ⤳ S [N "dst + 1", V "src + 1", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , SWAP 1                              -- ⤳ S [V "src + 1", V "dst + 1", V "size - 1", Ret, V "excl_dst", V "excl_dst_size"]
    , SWAP 2                              -- ⤳ S [V "size - 1", V "dst + 1", V "src + 1", Ret, V "excl_dst", V "excl_dst_size"]
    , Nop                                 -- ⤳ S ["size - 1" `Rename` "size", "dst + 1" `Rename` "dst", "src + 1" `Rename` "src", Ret, V "excl_dst", V "excl_dst_size"]
    , TagJump "memcpy_excl_loop"          -- ⤳ Ø
    , TagJumpdest "memcpy_excl_end"       -- ⤳ S [V "size", V "dst", V "src", Ret, V "excl_dst", V "excl_dst_size"]
    , POP                                 -- ⤳ S [V "dst", V "src", Ret, V "excl_dst", V "excl_dst_size"]
    , POP                                 -- ⤳ S [V "src", Ret, V "excl_dst", V "excl_dst_size"]
    , POP                                 -- ⤳ S [Ret, V "excl_dst", V "excl_dst_size"]
    , SWAP 2                              -- ⤳ S [V "excl_dst_size", V "excl_dst", Ret]
    , POP                                 -- ⤳ S [V "excl_dst", Ret]
    , POP                                 -- ⤳ S [Ret]
    , ProcedureReturn                     -- ⤳ Ø
    ]

procReturn = Procedure "return" [V "offset", V "size"] Ø
    [ TagJumpdest "return"          -- ⤳ S [Ret, V "offset", V "size"]
    , POP                           -- ⤳ S [V "offset", V "size"]
    , Push (memoryStashSize - 0x20) -- ⤳ S [N "memory stash size - 0x20", V "offset", V "size"]
    , ADD                           -- ⤳ S [N "offset + memory stash size - 0x20", V "size"]
    , DUP 2                         -- ⤳ S [V "size", V "offset + memory stash size - 0x20", V "size"]
    , DUP 2                         -- ⤳ S [V "offset + memory stash size - 0x20", V "size", V "offset + memory stash size - 0x20", V "size"]
    , MSTORE                        -- ⤳ S [V "offset + memory stash size - 0x20", V "size"]
    , SWAP 1                        -- ⤳ S [V "size", V "offset + memory stash size - 0x20"]
    , Push 0x20                     -- ⤳ S [N "0x20", V "size", V "offset + memory stash size - 0x20"]
    , ADD                           -- ⤳ S [N "size + 0x20", V "offset + memory stash size - 0x20"]
    , SWAP 1                        -- ⤳ S [V "offset + memory stash size - 0x20", V "size + 0x20"]
    , RETURN                        -- ⤳ Ø
    ]

procInit :: Procedure
procInit = Procedure "init" [] (S [])
    [ TagJumpdest "init"            -- ⤳ S [Ret]
    -- Require that caller is mc or self (viper uses CALL even for contract-internal calls)
    , CALLER                        -- ⤳ S [N "caller", Ret]
    , ProcedureCall "mc_address"    -- ⤳ S [N "mc_address", V "caller", Ret]
    , EQ                            -- ⤳ S [N "mc == caller", Ret]
    , ADDRESS                       -- ⤳ S [N "self", V "mc == caller", Ret]
    , CALLER                        -- ⤳ S [N "caller", V "self", V "mc == caller", Ret]
    , EQ                            -- ⤳ S [N "self == caller", V "mc == caller", Ret]
    , OR                            -- ⤳ S [N "self == caller | mc == caller", Ret]
    , ISZERO                        -- ⤳ S [N "!(self == caller | mc == caller)", Ret]
    , PC                            -- ⤳ S [N "invalid jump destination", V "!(self == caller | mc == caller)", Ret]
    , JUMPI                         -- ⤳ S [Ret]
    -- Require that call value is 0
    , CALLVALUE                     -- ⤳ S [N "callvalue", Ret]
    , PC                            -- ⤳ S [N "invalid jump destination", V "callvalue", Ret]
    , JUMPI                         -- ⤳ S [Ret]
    -- Require that call data size is at least 2 evm words
    , Push 0x40                     -- ⤳ S [N "0x40", Ret]
    , CALLDATASIZE                  -- ⤳ S [N "call data size", V "0x40", Ret]
    , LT                            -- ⤳ S [N "call data size < 0x40", Ret]
    , PC                            -- ⤳ S [N "invalid jump destination", V "call data size < 0x40", Ret]
    , JUMPI                         -- ⤳ S [Ret]
    -- Write to last word of memoryStash so that MSIZE never underflows.
    , Push (memoryStashSize - 0x20) -- ⤳ S [N "offset of last word of stash", Ret]
    , DUP 1                         -- ⤳ S [V "offset of last word of stash", V "offset of last word of stash", Ret]
    , MSTORE                        -- ⤳ S [Ret]
    , ProcedureReturn               -- ⤳ Ø
    ]

procDie :: Procedure
procDie = Procedure "die" [] Ø
    [ TagJumpdest "die" -- ⤳ S [Ret]
    , Push 0x20         -- ⤳ S [N "0x20", Ret]
    , Push 0            -- ⤳ S [N "0", V "0x20", Ret]
    , DUP 1             -- ⤳ S [V "0", V"0", V "0x20", Ret]
    , NOT               -- ⤳ S [N "2^256-1", V"0", V "0x20", Ret]
    , DUP 2             -- ⤳ S [V "0", V "2^256-1", V"0", V "0x20", Ret]
    , MSTORE            -- ⤳ S [V"0", V "0x20", Ret]
    , RETURN            -- ⤳ Ø
    ]

procMcAddress :: Integer -> Procedure
procMcAddress mc = Procedure "mc_address" [] (S [V "mc_address"])
    [ TagJumpdest "mc_address" -- ⤳ S [Ret]
    , Push mc                  -- ⤳ S [N "mc_address", Ret]
    , SWAP 1                   -- ⤳ S [Ret, V "mc_address"]
    , ProcedureReturn          -- ⤳ Ø
    ]

-}
