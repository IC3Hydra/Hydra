module EVM.GenericInitcode
( genericInitcode
) where

import Control.Monad
import qualified Data.ByteString as B (length)
import EVM.Bytecode
import Util

genericInitcode :: [Opcode]
genericInitcode = fromRight . consError "generic initcode" $
                  do let size = 15
                     let ops = [ PC          -- [0]
                               , CALLVALUE   -- [value, 0]
                               , PC          -- [PC, value, 0]
                               , JUMPI       -- [0]
                               , PUSH 1 size -- [header size, 0]
                               , DUP 1       -- [header size, header size, 0]
                               , CODESIZE    -- [code size, header size, header size, 0]
                               , SUB         -- [payload size, header size, 0]
                               , DUP 1       -- [payload size, payload size, header size, 0]
                               , DUP 3       -- [header size, payload size, payload size, header size, 0]
                               , DUP 5       -- [0, header size, payload size, payload size, header size, 0]
                               , CODECOPY    -- [payload size, header size, 0]
                               , DUP 3       -- [0, payload size, header size, 0]
                               , RETURN
                               ]
                     let assembledSize = fromIntegral . B.length . assemble $ ops
                     when (assembledSize /= size) $ Left $ "wrong size:" ++ show assembledSize
                     return ops
