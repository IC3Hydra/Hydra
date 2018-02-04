import           Test.QuickCheck.Gen
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
--import           Test.Tasty.SmallCheck as SC

import qualified Data.ByteString       as ByteString
import           Data.List
import           Data.Ord
import           Data.Word
import           EVM.Bytecode          as BC
import           EVM.Instrumentation


instance Arbitrary ByteString.ByteString where
    arbitrary = ByteString.pack <$> QC.listOf QC.arbitrary

instance Arbitrary BC.Opcode where
    arbitrary = QC.frequency [(10, simple), (1, push), (1, swap), (1, dup), (1, unknown)]
        where simple = word8ToOpcode <$> chooseAny `QC.suchThat` (isSimple . word8ToOpcode)
              isSimple (PUSH _ _)  = False
              isSimple (DUP _)     = False
              isSimple (SWAP _)    = False
              isSimple (Unknown _) = False
              isSimple _           = True
              push = do width <- QC.choose (1, 32)
                        val <- QC.choose (256^(width - 1) - 1, 256^width - 1)
                        return $ BC.PUSH width val
              swap = BC.SWAP <$> QC.choose (1,16)
              dup = BC.DUP <$> QC.choose (1,16)
              unknown = word8ToOpcode <$> chooseAny `QC.suchThat` (isUnknown . word8ToOpcode)
              isUnknown (Unknown _) = True
              isUnknown _           = False

main :: IO ()
main = defaultMain properties

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps = testGroup "(byte code)"
    [ QC.testProperty "word8ToOpcodeInverse" prop_word8ToOpcodeInverse
    , QC.testProperty "opcodeToWord8Inverse" prop_opcodeToWord8Inverse
    , QC.testProperty "parseInverse" prop_parseInverse
    , QC.testProperty "assembleInverse" prop_assembleInverse
    , QC.testProperty "hexStringToByteStringInverse" prop_hexStringToByteStringInverse
    , QC.testProperty "instrumentFirstRuns" prop_instrumentFirstRuns
    , QC.testProperty "instrumentNthRuns" prop_instrumentNthRuns
    ]

prop_word8ToOpcodeInverse :: Word8 -> Bool
prop_word8ToOpcodeInverse w = w == (BC.opcodeToWord8 . BC.word8ToOpcode) w

prop_opcodeToWord8Inverse :: Opcode -> Bool
prop_opcodeToWord8Inverse op = op `equal` (BC.word8ToOpcode . BC.opcodeToWord8) op
    where equal (PUSH n _) (PUSH n' _) = n == n'
          equal op         op'         = op == op'

prop_parseInverse :: ByteString.ByteString -> Property
prop_parseInverse b = isRight parsed ==> b == (assemble . fromRight) parsed
    where parsed = parse b
          isRight = either (const False) (const True)
          fromRight = either (error "Left") id

prop_assembleInverse :: [BC.Opcode] -> Bool
prop_assembleInverse ops =
    case (BC.parse . BC.assemble) ops of
        (Left _)     -> False
        (Right ops') -> ops == ops'

prop_hexStringToByteStringInverse :: ByteString.ByteString -> Bool
prop_hexStringToByteStringInverse bs =
    bs == (BC.hexStringToByteString . BC.byteStringToHexString) bs

prop_instrumentFirstRuns :: Integer -> [BC.Opcode] -> Property
prop_instrumentFirstRuns addr ops = addr <= (2^256-1) ==> either (const True) (const True) (instrumentFirst addr ops)

prop_instrumentNthRuns :: Integer -> [BC.Opcode] -> Property
prop_instrumentNthRuns addr ops = addr <= (2^256-1) ==> either (const True) (const True) (instrumentNth addr ops)
