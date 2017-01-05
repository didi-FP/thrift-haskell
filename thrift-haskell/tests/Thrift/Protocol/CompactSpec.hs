{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}
module Thrift.Protocol.CompactSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.ByteString.Lazy as BL
import Data.Word                         (Word8)
import Data.Binary.Get                   (runGet)
import Data.Binary.Put                   (runPut)
import Data.Binary.Parser                (parseLazy)
import Thrift.Protocol.Compact           (int32ToZigZag, zigZagToInt32, int64ToZigZag, zigZagToInt64,
                                          getVarInt16le, getVarInt32le, getVarInt64le,
                                          putVarInt16le, putVarInt32le, putVarInt64le)
import Thrift.Protocol.Compact           (compactProtocol)
import Thrift.Type
import Thrift.Arbitrary ()

encode :: TValue -> BL.ByteString
encode = runPut . encodeTValue compactProtocol

decode :: TypeCode -> BL.ByteString -> Either String TValue
decode tc = parseLazy (decodeTValue compactProtocol tc)

encodeMsg :: Message -> BL.ByteString
encodeMsg = runPut . encodeMessage  compactProtocol

decodeMsg :: TypeCode -> BL.ByteString -> Either String Message
decodeMsg tc = parseLazy (decodeMessage compactProtocol tc)

-- | For each given pair, verifies that parsing the byte array yields the
-- value, and that serializing the value yields the byte array.
readWriteCases :: [([Word8], TValue)] -> Expectation
readWriteCases = mapM_ $ \ (bytes, value) -> do
    let bs = BL.pack bytes
    decode (tValueTC value) bs  `shouldBe` Right value
    encode value `shouldBe` bs

readMessageCases :: [([Word8], Message)] -> Expectation
readMessageCases = mapM_ $ \ (bytes, msg) ->
    decodeMsg (tValueTC $ messagePayload msg) (BL.pack bytes)  `shouldBe` Right msg

readWriteMessageCases :: [([Word8], Message)] -> Expectation
readWriteMessageCases = mapM_ $ \ (bytes, msg) -> do
    let bs = BL.pack bytes
    decodeMsg (tValueTC $ messagePayload msg) bs  `shouldBe` Right msg
    decodeMsg (tValueTC $ messagePayload msg) (encodeMsg msg) `shouldBe` Right msg

-- | For each pair, verifies that if the given TType is parsed, the request
-- fails to parse because the type ID was invalid.
invalidTypeIDCases :: [(TypeCode, [Word8])] -> Expectation
invalidTypeIDCases = mapM_ $ \(tc, bs) ->
    case decode tc (BL.pack bs) :: Either String TValue of
        Right v -> expectationFailure $
          "Expected " ++ show bs ++ " to fail to parse. " ++
          "Got: " ++ show v
        Left msg -> msg `shouldContain` "Unknown compact field type code"

-- | For each pair, verifies that if the given TType is parsed, the request
-- fails to parse lecause the input was too short.
tooShortCases :: [(TypeCode, [Word8])] -> Expectation
tooShortCases = mapM_ $ \ (tc, bs) ->
    case decode tc (BL.pack bs) :: Either String TValue of
        Right v -> expectationFailure $
          "Expected " ++ show bs ++ " to fail to parse. " ++
          "Got: " ++ show v
        Left msg -> msg `shouldContain` "not enough bytes"

spec :: Spec
spec = describe "CompactProtocol" $ do

    prop "convert letween 32-bit integer and zigzag" $ \a ->
        (zigZagToInt32 . int32ToZigZag) a === a

    prop "convert letween 64-bit integer and zigzag" $ \a ->
        (zigZagToInt64 . int64ToZigZag) a === a

    prop "serialize and deserialize 16-bit var int in zigzag format" $ \a ->
        (runGet getVarInt16le . runPut . putVarInt16le) a === a

    prop "serialize and deserialize 32-bit var int in zigzag format" $ \a ->
        (runGet getVarInt32le . runPut . putVarInt32le) a === a

    prop "serialize and deserialize 64-bit var int in zigzag format" $ \a ->
        (runGet getVarInt64le . runPut . putVarInt64le) a === a

    {- prop "can roundtrip values" $ \val ->
        let compareTValue ev1 ev2 = case ev1 of
                Right (TMap kt1 vt1 x1) -> if null x1
                                              then case ev2 of
                                                     Right (TMap _ _ x2) -> x1 === x2
                                                     _                   -> ev1 === ev2
                                              else ev1 === ev2
                _                       -> ev1 === ev2
             in compareTValue (decode (tValueTC val) (encode val)) (Right val)

    prop "can roundtrip messages" $ \msg ->
       decodeMsg (tValueTC $ messagePayload msg) (encodeMsg msg) === Right msg
    -}

    it "can read and write booleans" $ readWriteCases
        [ ([0x01], TBool True)
        , ([0x02], TBool False)
        ]

    it "can read and write binary" $ readWriteCases
        [ ([ 0x00 ], TBinary "")
        , ([ 0x05                          -- length = 5
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
        [ ([0x02],             TInt16 1)
        , ([0xfe, 0x03],       TInt16 255)
        , ([0x80, 0x04],       TInt16 256)
        , ([0x82, 0x04],       TInt16 257)
        , ([0xfe, 0xff, 0x03], TInt16 32767)
        , ([0x01],             TInt16 -1)
        , ([0x03],             TInt16 -2)
        , ([0xff, 0x03],       TInt16 -256)
        , ([0xfd, 0x03],       TInt16 -255)
        , ([0xff, 0xff, 0x03], TInt16 -32768)
        ]

    it "can read and write 32-bit integers" $ readWriteCases
        [ ([0x02],                         TInt32 1)
        , ([0xfe, 0x03],                   TInt32 255)
        , ([0xfe, 0xff, 0x07],             TInt32 65535)
        , ([0xfe, 0xff, 0xff, 0x0f],       TInt32 16777215)
        , ([0xfe, 0xff, 0xff, 0xff, 0x0f], TInt32 2147483647)
        , ([0x01],                         TInt32 -1)
        , ([0xff, 0x03],                   TInt32 -256)
        , ([0xff, 0xff, 0x07],             TInt32 -65536)
        , ([0xff, 0xff, 0xff, 0x0f],       TInt32 -16777216)
        , ([0xff, 0xff, 0xff, 0xff, 0x0f], TInt32 -2147483648)
        ]

    it "can read and write 64-bit integers" $ readWriteCases
        [ ([0x02],                                           TInt64 1)
        , ([0xfe, 0xff, 0xff, 0xff, 0x1f],                   TInt64 4294967295)
        , ([0xfe, 0xff, 0xff, 0xff, 0xff, 0x3f],             TInt64 1099511627775)
        , ([0xfe, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f],       TInt64 281474976710655)
        , ([0xfe, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01], TInt64 72057594037927935)
        , ([0xfe, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01], TInt64 9223372036854775807)
        , ([0x01],                                           TInt64 -1)
        , ([0xff, 0xff, 0xff, 0xff, 0x1f],                   TInt64 -4294967296)
        , ([0xff, 0xff, 0xff, 0xff, 0xff, 0x3f],             TInt64 -1099511627776)
        , ([0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f],       TInt64 -281474976710656)
        , ([0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01], TInt64 -72057594037927936)
        , ([0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01], TInt64 -9223372036854775808)
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

        , ([ 0x15                    -- ttype = i32, field ID = 1
           , 0x54                    -- 42
           , 0x00                    -- stop
           ], TStruct [(1, TInt32 42)])

        , ([ 0x11                    -- ttype = bool true, field ID = 1
           , 0x52                    -- ttype = bool false, field ID = 6
           , 0x23, 0x2a              -- ttype = byte, field ID = 8, byte 42
           , 0x03, 0x40, 0x2b        -- ttype = byte, field ID = 32, byte 42
           , 0x00                    -- stop
           ], TStruct [(1, TBool True), (6, TBool False), (8, TInt8 42), (32, TInt8 43)])

        , ([ 0x29                    -- ttype = list, field ID = 2
           , 0x28
           , 0x03, 0x66, 0x6f, 0x6f  -- "foo"
           , 0x03, 0x62, 0x61, 0x72  -- "bar"
           , 0x00                    -- stop
           ], TStruct [(2, TList TC_Binary [TBinary "foo", TBinary "bar"])])
        ]


    it "can read and write maps" $ readWriteCases
        [ {-
          ([ 0x00
           ], TMap TC_Bool TC_Int8 [])
        , -}
          ([ 0x01, 0x89                    -- ktype = binary, vtype = list

           -- "world"
           , 0x05                          -- length = 5
           , 0x77, 0x6f, 0x72, 0x6c, 0x64  -- world

           -- [1, 2, 3]
           , 0x33                          -- type = byte, count = 3
           , 0x01, 0x02, 0x03              -- 1, 2, 3
           ], TMap TC_Binary TC_List
               [ (TBinary "world", TList TC_Int8 [TInt8 1, TInt8 2, TInt8 3])
               ])
        ]

    it "can read and write sets" $ readWriteCases
        [ ([0x01
           ], TSet TC_Bool [])
        , ([ 0x11, 0x01
           ], TSet TC_Bool [TBool True])
        ]

    it "can read and write lists" $ readWriteCases
        [ ([0x01
           ], TList TC_Bool [])
        , ([ 0x51, 0x01, 0x02, 0x02, 0x01, 0x01
           ], TList TC_Bool
               [ TBool True, TBool False, TBool False, TBool True, TBool True])
        ]
    it "fails if the input is too short" $ tooShortCases
        [ (TC_Bool, [])
        , (TC_Int8, [])
        , (TC_Int16, [])
        , (TC_Int32, [])
        , (TC_Int64, [])
        , (TC_Double, [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07])
        , (TC_Binary, [0x01])

        , (TC_Map, [0x02])
        , (TC_Map, [0x02, 0x33])
        , (TC_Map, [0x02, 0x33, 0x01])

        , (TC_Set, [0x2a])
        , (TC_Set, [0x2a, 0x33, 0x00])

        , (TC_List, [0x29])
        , (TC_List, [0x29, 0x33])
        ]

    it "denies invalid type IDs" $ invalidTypeIDCases
        [ (TC_Struct, [0x0d, 0x00, 0x01])
        , (TC_Map, [0x1a, 0xd1, 0x00])
        , (TC_Set, [0x1d])
        , (TC_List, [0x1d])
        ]

    it "can read and write messages" $ readWriteMessageCases
        [ ([ 0x82                                   -- Protocol id
           , 0x21                                   -- Version and Type = Call
           , 0x2a                                   -- seqId = 42
           , 0x06                                   -- name length = 6
           , 0x67, 0x65, 0x74, 0x46, 0x6f, 0x6f     -- 'getFoo'
           , 0x00                                   -- stop
           ], Message "getFoo" MT_Call 42 (TStruct []))
        , ([ 0x82                                   -- Protocol id
           , 0x41                                   -- Version and Type = Reply
           , 0x01                                   -- seqId = 01
           , 0x06                                   -- name length = 6
           , 0x73, 0x65, 0x74, 0x42, 0x61, 0x72     -- 'setBar'
           , 0x00                                   -- stop
           ], Message "setBar" MT_Reply 1 (TStruct []))
        ]
