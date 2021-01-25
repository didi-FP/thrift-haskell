{-# LANGUAGE MultiWayIf #-}
-- |
-- Module      :  Thrift.Protocol.Binary
-- Copyright   :  (c) Winterland 2016
-- License     :  BSD3
--
-- Maintainer  :  Winterland <drkoster@qq.com>
-- Stability   :  experimental
--
-- Thrift Binary Protocol.
--
module Thrift.Protocol.Binary where


import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Data.Bits
import Data.ByteString     (ByteString)
import Data.Int
import Data.Word
import Thrift.Type

import qualified Data.ByteString     as B
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.Text.Encoding  as TE
import qualified Z.Data.Parser       as P
import qualified Z.Data.Builder      as B


------------------------------------------------------------------------------

-- | Provides an implementation of the Thrift Binary Protocol.
binaryProtocol :: Protocol
binaryProtocol = Protocol encodeMessage encodeTValue decodeMessage decodeTValue

versionMask :: Word32
versionMask = 0xffff0000

version1 :: Word32
version1 = 0x80010000

decodeLength :: Parser Int
decodeLength = fromIntegral <$> P.decodePrimBE @Int32

encodeLength :: Int -> Builder ()
encodeLength = B.encodePrimBE@ Int32 . fromIntegral

------------------------------------------------------------------------------

encodeMessage :: Message -> Builder ()
encodeMessage msg = do
    B.encodePrimBE (version1 .|. fromIntegral (messageType msg))
    let name = T.getUTF8Bytes msg
    encodeLength (B.length name)
    B.bytes name
    B.encodePrimBE (messageId msg)
    encodeTValue (messagePayload msg)

decodeMessage :: TypeCode -> Parser Message
decodeMessage typ = do
    size <- P.decodePrimBE @Int32
    if size < 0
    then parseStrict size
    else parseNonStrict size
  where
    -- versionAndType:4 name~4 seqid:4 payload
    -- versionAndType = version:2 0x00 type:1
    parseStrict versionAndType = do
        let version = versionMask .&. fromIntegral versionAndType
            tcode = fromIntegral (0x00000007 .&. versionAndType)
        when (version /= version1) (P.fail' $ "Unsupported version: " <> T.toText version)
        nlen <- P.decodePrimBE @Int32
        Message <$> (T.validate <$> P.take (fromIntegral nlen))
                <*> pure tcode
                <*> P.decodePrimBE @Int32
                <*> decodeTValue (if tcode == MT_Exception
                                then getTypeCode (typeCode :: TypeCodeTagged AppException)
                                else typ)

    -- name~4 type:1 seqid:4 payload
    parseNonStrict nlen =
        Message <$> (T.validate <$> P.take (fromIntegral nlen))
                <*> ((0x07 .&.) <$> P.anyWord8)
                <*> P.decodePrimBE @Int32
                <*> decodeTValue typ

------------------------------------------------------------------------------

encodeTValue :: TValue -> Builder ()
encodeTValue v = case v of
    (TBinary a) -> do encodeLength (B.length a)
                      B.bytes a
    (TBool   a) -> B.word8 (if a then 1 else 0)
    (TInt8   a) -> B.encodePrim a
    (TDouble a) -> B.encodePrimBE a
    (TInt16  a) -> B.encodePrimBE a
    (TInt32  a) -> B.encodePrimBE a
    (TInt64  a) -> B.encodePrimBE a
    (TStruct a) -> do
        forM_ a $ \ (fid, val) -> do
            B.word8 (tValueTC val)
            B.encodePrimBE fid
            encodeTValue val
        B.word8 TC_Stop

    (TList t a) -> do
        B.word8 t
        encodeLength (length a)
        mapM_ encodeTValue a

    (TMap tk tv a) -> do
        B.word8 tk
        B.word8 tv
        encodeLength (length a)
        forM_ a $ \ (key, val) -> do
            encodeTValue key
            encodeTValue val

    (TSet t a) -> do
        B.word8 t
        encodeLength (length a)
        mapM_ encodeTValue a

decodeTValue :: Int8 -> Parser TValue
decodeTValue TC_Bool   = TBool . (/=0) <$> P.decodePrim
decodeTValue TC_Int8   = TInt8 <$> P.decodePrim
decodeTValue TC_Double = TDouble <$> P.decodePrimBE @Double
decodeTValue TC_Int16  = TInt16 <$> P.decodePrimBE @Int16
decodeTValue TC_Int32  = TInt32 <$> P.decodePrimBE @Int32
decodeTValue TC_Int64  = TInt64 <$> P.decodePrimBE @Int64
decodeTValue TC_Binary = TBinary <$> (decodeLength >>= P.take)
decodeTValue TC_Struct = TStruct <$> go
  where
    go :: Parser [(Int, TValue)]
    go = do
        typ <- P.anyWord8
        if typ == TC_Stop
        then return []
        else do
            fid <- P.decodePrimBE
            val <- decodeTValue typ
            rest <- go
            return ((fid, val) : rest)

decodeTValue TC_Map    = do kt <- P.decodePrim
                         vt <- P.decodePrim
                         len <- decodeLength
                         TMap kt vt <$>
                            replicateM len ((,) <$> decodeTValue kt <*> decodeTValue vt)
decodeTValue TC_Set    = do vt <- P.decodePrim
                         len <- decodeLength
                         TSet vt <$> replicateM len (decodeTValue vt)
decodeTValue TC_List   = do vt <- P.decodePrim
                         len <- decodeLength
                         TList vt <$> replicateM len (decodeTValue vt)

decodeTValue tc        = fail $ "bad type code: " ++ show tc
