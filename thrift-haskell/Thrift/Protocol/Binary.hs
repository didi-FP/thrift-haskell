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
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.IEEE754


------------------------------------------------------------------------------

-- | Provides an implementation of the Thrift Binary Protocol.
binaryProtocol :: Protocol
binaryProtocol = Protocol putMessage putTValue getMessage getTValue

versionMask :: Word32
versionMask = 0xffff0000

version1 :: Word32
version1 = 0x80010000

getLength :: Get Int
getLength = fromIntegral <$> getInt32be

putLength :: Int -> Put
putLength = putInt32be . fromIntegral

------------------------------------------------------------------------------

putMessage :: Message -> Put
putMessage msg = do
    putWord32be (version1 .|. fromIntegral (messageType msg))
    let name = TE.encodeUtf8 $ messageName msg
    putLength (B.length name)
    putByteString name
    putInt32be (messageId msg)
    putTValue (messagePayload msg)

getMessage :: TypeCode -> Get Message
getMessage typ = do
    size <- getInt32be
    if size < 0
    then parseStrict size
    else parseNonStrict size
  where
    -- versionAndType:4 name~4 seqid:4 payload
    -- versionAndType = version:2 0x00 type:1
    parseStrict versionAndType = do
        let version = versionMask .&. fromIntegral versionAndType
            tcode = fromIntegral (0x00000007 .&. versionAndType)
        when (version /= version1) (fail $ "Unsupported version: " ++ show version)
        nlen <- getInt32be
        Message <$> (TE.decodeUtf8 <$> getByteString (fromIntegral nlen))
                <*> pure tcode
                <*> getInt32be
                <*> getTValue (if tcode == MT_Exception
                                then getTypeCode (typeCode :: TypeCodeTagged AppException)
                                else typ)

    -- name~4 type:1 seqid:4 payload
    parseNonStrict nlen =
        Message <$> (TE.decodeUtf8 <$> getByteString (fromIntegral nlen))
                <*> ((0x07 .&.) <$> getInt8)
                <*> getInt32be
                <*> getTValue typ

------------------------------------------------------------------------------

putTValue :: TValue -> Put
putTValue v = case v of
    (TBinary a) -> do putLength (B.length a)
                      putByteString a
    (TBool   a) -> putInt8 (if a then 1 else 0)
    (TInt8   a) -> putInt8 a
    (TDouble a) -> putDoublebe a
    (TInt16  a) -> putInt16be a
    (TInt32  a) -> putInt32be a
    (TInt64  a) -> putInt64be a
    (TStruct a) -> do
        forM_ a $ \ (fid, val) -> do
            putInt8 (tValueTC val)
            putInt16be (fromIntegral fid)
            putTValue val
        putInt8 TC_Stop

    (TList t a) -> do
        putInt8 t
        putLength (length a)
        mapM_ putTValue a

    (TMap tk tv a) -> do
        putInt8 tk
        putInt8 tv
        putLength (length a)
        forM_ a $ \ (key, val) -> do
            putTValue key
            putTValue val

    (TSet t a) -> do
        putInt8 t
        putLength (length a)
        mapM_ putTValue a

getTValue :: Int8 -> Get TValue
getTValue TC_Bool   = TBool . (/=0) <$> getWord8
getTValue TC_Int8   = TInt8 <$> getInt8
getTValue TC_Double = TDouble <$> getDoublebe
getTValue TC_Int16  = TInt16 <$> getInt16be
getTValue TC_Int32  = TInt32 <$> getInt32be
getTValue TC_Int64  = TInt64 <$> getInt64be
getTValue TC_Binary = TBinary <$> (getLength >>= getByteString)
getTValue TC_Struct = TStruct <$> go
  where
    go :: Get [(Int, TValue)]
    go = do
        typ <- getInt8
        if typ == TC_Stop
        then return []
        else do
            fid <- getInt16be
            val <- getTValue typ
            rest <- go
            let fid' = fromIntegral fid
            return ((fid', val) : rest)

getTValue TC_Map    = do kt <- getInt8
                         vt <- getInt8
                         len <- getLength
                         TMap kt vt <$>
                            replicateM len ((,) <$> getTValue kt <*> getTValue vt)
getTValue TC_Set    = do vt <- getInt8
                         len <- getLength
                         TSet vt <$> replicateM len (getTValue vt)
getTValue TC_List   = do vt <- getInt8
                         len <- getLength
                         TList vt <$> replicateM len (getTValue vt)

getTValue tc        = fail $ "bad type code: " ++ show tc
