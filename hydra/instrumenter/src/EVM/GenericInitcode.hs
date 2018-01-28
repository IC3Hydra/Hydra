module EVM.GenericInitcode
( genericInitcode
) where

import Control.Monad
import qualified Data.ByteString as B (length)
import EVM.Bytecode
import EVM.BytecodePlus
import Util

genericInitcode :: [Opcode]
genericInitcode = fromRight . consError "generic initcode" $
                  do let size = 15
                     let ops = [ PC          -- ⤳ S [N "0"]
                               , CALLVALUE   -- ⤳ S [N "value", V "0"]
                               , PC          -- ⤳ S [N "PC", V "value", V "0"]
                               , JUMPI       -- ⤳ S [V "0"]
                               , PUSH 1 size -- ⤳ S [N "header size", V "0"]
                               , DUP 1       -- ⤳ S [V "header size", V "header size", V "0"]
                               , CODESIZE    -- ⤳ S [N "code size", V "header size", V "header size", V "0"]
                               , SUB         -- ⤳ S [N "payload size", V "header size", V "0"]
                               , DUP 1       -- ⤳ S [V "payload size", V "payload size", V "header size", V "0"]
                               , DUP 3       -- ⤳ S [V "header size", V "payload size", V "payload size", V "header size", V "0"]
                               , DUP 5       -- ⤳ S [V "0", V "header size", V "payload size", V "payload size", V "header size", V "0"]
                               , CODECOPY    -- ⤳ S [V "payload size", V "header size", V "0"]
                               , DUP 3       -- ⤳ S [V "0", V "payload size", V "header size", V "0"]
                               , RETURN      -- ⤳ Ø
                               ]
                     let assembledSize = fromIntegral . B.length . assemble $ ops
                     when (assembledSize /= size) $ Left $ "wrong size:" ++ show assembledSize
                     return ops
