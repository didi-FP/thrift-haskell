{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Thrift.Protocol.BinarySpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.ByteString.Lazy as BL
import Data.Word             (Word8)
import Data.Binary.Put           (runPut)
import Data.Binary.Parser        (parseLazy)
import Thrift.Protocol.Binary  (binaryProtocol)
import Thrift.Type
import Thrift.Arbitrary ()

encode :: TValue -> BL.ByteString
encode = runPut . encodeTValue binaryProtocol

decode :: TypeCode -> BL.ByteString -> Either String TValue
decode tc = parseLazy (decodeTValue binaryProtocol tc)

encodeMsg :: Message -> BL.ByteString
encodeMsg = runPut . encodeMessage  binaryProtocol

decodeMsg :: TypeCode -> BL.ByteString -> Either String Message
decodeMsg tc = parseLazy (decodeMessage binaryProtocol tc)


-- | For each given pair, verifies that parsing the byte array yields the
-- value, and that serializing the value yields the byte array.
readWriteCases :: [([Word8], TValue)] -> Expectation
readWriteCases = mapM_ $ \ (bytes, value) -> do
    let bs = BL.pack bytes
    decode (tValueTC value) bs  `shouldBe` Right value
    encode value `shouldBe` bs


readWriteMessageCases :: [([Word8], Message)] -> Expectation
readWriteMessageCases = mapM_ $ \ (bytes, msg) -> do
    let bs = BL.pack bytes
    decodeMsg (tValueTC $ messagePayload msg) bs  `shouldBe` Right msg
    decodeMsg (tValueTC $ messagePayload msg) (encodeMsg msg) `shouldBe` Right msg

readMessageCases :: [([Word8], Message)] -> Expectation
readMessageCases = mapM_ $ \ (bytes, msg) ->
    decodeMsg (tValueTC $ messagePayload msg) (BL.pack bytes)  `shouldBe` Right msg

-- | For each pair, verifies that if the given TType is parsed, the request
-- fails to parse because the input was too short.
tooShortCases :: [(TypeCode, [Word8])] -> Expectation
tooShortCases = mapM_ $ \ (tc, bs) ->
    case decode tc (BL.pack bs) :: Either String TValue of
        Right v -> expectationFailure $
          "Expected " ++ show bs ++ " to fail to parse. " ++
          "Got: " ++ show v
        Left msg -> msg `shouldContain` "not enough bytes"

spec :: Spec
spec = describe "BinaryProtocol" $ do

    prop "can roundtrip values" $ \ someVal ->
        decode (tValueTC someVal) (encode someVal)
            === Right someVal

    prop "can roundtrip messages" $ \ msg ->
        decodeMsg (tValueTC $ messagePayload msg) (encodeMsg msg) === Right msg

    it "can read and write booleans" $ readWriteCases
        [ ([0x01], TBool True)
        , ([0x00], TBool False)
        ]

    it "can read and write binary" $ readWriteCases
        [ ([ 0x00, 0x00, 0x00, 0x00 ], TBinary "")
        , ([ 0x00, 0x00, 0x00, 0x05        -- length = 5
           , 0x68, 0x65, 0x6c, 0x6c, 0x6f  -- hello
           ], TBinary "hello")
        ]

    it "can read and write 8-bit integers" $ readWriteCases
        [ ([0x01], TInt8 1)
        , ([0x05], TInt8 5)
        , ([0x7f], TInt8 127)
        , ([0xff], TInt8 -1)
        , ([0x80], TInt8 -128)
        ]

    it "can read and write 16-bit integers" $ readWriteCases
        [ ([0x00, 0x01], TInt16 1)
        , ([0x00, 0xff], TInt16 255)
        , ([0x01, 0x00], TInt16 256)
        , ([0x01, 0x01], TInt16 257)
        , ([0x7f, 0xff], TInt16 32767)
        , ([0xff, 0xff], TInt16 -1)
        , ([0xff, 0xfe], TInt16 -2)
        , ([0xff, 0x00], TInt16 -256)
        , ([0xff, 0x01], TInt16 -255)
        , ([0x80, 0x00], TInt16 -32768)
        ]

    it "can readd and write 32-bit integers" $ readWriteCases
        [ ([0x00, 0x00, 0x00, 0x01], TInt32 1)
        , ([0x00, 0x00, 0x00, 0xff], TInt32 255)
        , ([0x00, 0x00, 0xff, 0xff], TInt32 65535)
        , ([0x00, 0xff, 0xff, 0xff], TInt32 16777215)
        , ([0x7f, 0xff, 0xff, 0xff], TInt32 2147483647)
        , ([0xff, 0xff, 0xff, 0xff], TInt32 -1)
        , ([0xff, 0xff, 0xff, 0x00], TInt32 -256)
        , ([0xff, 0xff, 0x00, 0x00], TInt32 -65536)
        , ([0xff, 0x00, 0x00, 0x00], TInt32 -16777216)
        , ([0x80, 0x00, 0x00, 0x00], TInt32 -2147483648)
        ]

    it "can readd and write 64-bit integers" $ readWriteCases
        [ ([0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01], TInt64 1)
        , ([0x00, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff], TInt64 4294967295)
        , ([0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff, 0xff], TInt64 1099511627775)
        , ([0x00, 0x00, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff], TInt64 281474976710655)
        , ([0x00, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff], TInt64 72057594037927935)
        , ([0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff], TInt64 9223372036854775807)
        , ([0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff], TInt64 -1)
        , ([0xff, 0xff, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00], TInt64 -4294967296)
        , ([0xff, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00], TInt64 -1099511627776)
        , ([0xff, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], TInt64 -281474976710656)
        , ([0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], TInt64 -72057594037927936)
        , ([0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], TInt64 -9223372036854775808)
        ]

    it "can read and write doubles" $ readWriteCases
        [ ([0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], TDouble 0.0)
        , ([0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00], TDouble 1.0)
        , ([0x3f, 0xf0, 0x00, 0x00, 0x00, 0x06, 0xdf, 0x38], TDouble 1.0000000001)
        , ([0x3f, 0xf1, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9a], TDouble 1.1)
        , ([0xbf, 0xf1, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9a], TDouble -1.1)
        , ([0x40, 0x09, 0x21, 0xfb, 0x54, 0x44, 0x2d, 0x18], TDouble 3.141592653589793)
        , ([0xbf, 0xf0, 0x00, 0x00, 0x00, 0x06, 0xdf, 0x38], TDouble -1.0000000001)
        ]

    it "can read and write structs" $ readWriteCases
        [ ([0x00], TStruct [])

        , ([ 0x08                    -- ttype = i32
           , 0x00, 0x01              -- field ID = 1
           , 0x00, 0x00, 0x00, 0x2a  -- 42
           , 0x00                    -- stop
           ], TStruct [(1, TInt32 42)])

        , ([ 0x0F       -- ttype = list
           , 0x00, 0x02 -- field ID = 2

           , 0x0B                    -- ttype binary
           , 0x00, 0x00, 0x00, 0x02  -- size = 2

           , 0x00, 0x00, 0x00, 0x03  -- length = 3
           , 0x66, 0x6f, 0x6f        -- foo

           , 0x00, 0x00, 0x00, 0x03  -- length = 3
           , 0x62, 0x61, 0x72        -- bar

           , 0x00
           ], TStruct
           [ (2, TList TC_Binary [TBinary "foo", TBinary "bar"])
           ])
        ]

    it "can read and write maps" $ readWriteCases
        [ ([ 0x02, 0x03              -- ktype = bool, vtype = byte
           , 0x00, 0x00, 0x00, 0x00  -- count = 0
           ], TMap TC_Bool TC_Int8 [])
        , ([ 0x0B, 0x0F              -- ktype = binary, vtype = list
           , 0x00, 0x00, 0x00, 0x01  -- count = 1

           -- "world"
           , 0x00, 0x00, 0x00, 0x05        -- length = 5
           , 0x77, 0x6f, 0x72, 0x6c, 0x64  -- world

           -- [1, 2, 3]
           , 0x03                          -- type = byte
           , 0x00, 0x00, 0x00, 0x03        -- count = 3
           , 0x01, 0x02, 0x03              -- 1, 2, 3
           ], TMap TC_Binary TC_List
           [ (TBinary "world", TList TC_Int8 [TInt8 1, TInt8 2, TInt8 3])
           ])
        ]

    it "can read and write sets" $ readWriteCases
        [ ([0x02, 0x00, 0x00, 0x00, 0x00
           ], TSet TC_Bool [])
        , ([ 0x02
           , 0x00, 0x00, 0x00, 0x01
           , 0x01
           ], TSet TC_Bool [TBool True])
        ]

    it "can read and write lists" $ readWriteCases
        [ ([0x02, 0x00, 0x00, 0x00, 0x00
           ], TList TC_Bool [])
        , ([ 0x02
           , 0x00, 0x00, 0x00, 0x05
           , 0x01, 0x00, 0x00, 0x01, 0x01
           ], TList TC_Bool
               [ TBool True
               , TBool False
               , TBool False
               , TBool True
               , TBool True
               ])
        ]

    it "fails if the input is too short" $ tooShortCases
        [ (TC_Bool, [])
        , (TC_Int8, [])
        , (TC_Int16, [0x01])
        , (TC_Int32, [0x01, 0x02, 0x03])
        , (TC_Int64, [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07])
        , (TC_Double, [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07])
        , (TC_Binary, [0x00, 0x00, 0x00])
        , (TC_Binary, [0x00, 0x00, 0x00, 0x01])
        , (TC_Binary, [0x00, 0x00, 0x00, 0x02, 0x01])

        , (TC_Map, [0x02])
        , (TC_Map, [0x02, 0x03])
        , (TC_Map, [0x02, 0x03, 0x00, 0x00, 0x00])
        , (TC_Map, [0x02, 0x03, 0x00, 0x00, 0x00, 0x01])
        , (TC_Map, [0x02, 0x03, 0x00, 0x00, 0x00, 0x01, 0x01])
        , (TC_Map,
           [0x02, 0x03, 0x00, 0x00, 0x00, 0x02, 0x01, 0x01, 0x01])

        , (TC_Set, [0x02])
        , (TC_Set, [0x02, 0x00, 0x00, 0x00])
        , (TC_Set, [0x02, 0x00, 0x00, 0x00, 0x01])
        , (TC_Set, [0x02, 0x00, 0x00, 0x00, 0x02, 0x01])

        , (TC_List, [0x02])
        , (TC_List, [0x02, 0x00, 0x00, 0x00])
        , (TC_List, [0x02, 0x00, 0x00, 0x00, 0x01])
        , (TC_List, [0x02, 0x00, 0x00, 0x00, 0x02, 0x01])
        ]

    it "can read and write messages" $ readWriteMessageCases
        [ ([ 0x00, 0x00, 0x00, 0x06                 -- length = 6
           , 0x67, 0x65, 0x74, 0x46, 0x6f, 0x6f     -- 'getFoo'
           , 0x01                                   -- type = Call
           , 0x00, 0x00, 0x00, 0x2a                 -- seqId = 42
           , 0x00                                   -- empty struct
           ], Message "getFoo" MT_Call 42 (TStruct [])),
          ([ 0x00, 0x00, 0x00, 0x06                 -- length = 6
           , 0x73, 0x65, 0x74, 0x42, 0x61, 0x72     -- 'setBar'
           , 0x02                                   -- type = Reply
           , 0x00, 0x00, 0x00, 0x01                 -- seqId = 1
           , 0x00
           ], Message "setBar" MT_Reply 1 (TStruct []))
        ]

    it "can read strict messages" $ readMessageCases
        [ ([ 0x80, 0x01     -- version = 1
           , 0x00, 0x03     -- type = Exception

           , 0x00, 0x00, 0x00, 0x06              -- length = 6
           , 0x67, 0x65, 0x74, 0x46, 0x6f, 0x6f  -- 'getFoo'

           , 0x00, 0x00, 0x00, 0x2a  -- seqId = 42

           , 0x02, 0x00, 0x01, 0x01  -- {1: True}
           , 0x00
           ], Message "getFoo" MT_Exception 42 (TStruct [(1, TBool True)]))
        , ([ 0x80, 0x01     -- version = 1
           , 0x00, 0x04     -- type = EXCEPTION

           , 0x00, 0x00, 0x00, 0x06              -- length = 6
           , 0x73, 0x65, 0x74, 0x42, 0x61, 0x72  -- 'setBar'

           , 0x00, 0x00, 0x00, 0x01  -- seqId = 1

           , 0x00
           ], Message "setBar" MT_Oneway 1 (TStruct []))
        ]
