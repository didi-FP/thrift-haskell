{-# LANGUAGE MultiWayIf #-}
-- |
-- Module      :  Pinch.Protocol.Binary
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- Implements the Thrift Binary Protocol as a 'Protocol'.
module Data.Thrift.Protocol.Binary where


import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Data.Bits
import Data.ByteString     (ByteString)
import Data.Int
import Data.Word

import qualified Data.ByteString     as B
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.Text.Encoding  as TE
import Data.Binary.Put
import Data.Binary.Get

import Data.Thrift.Type

------------------------------------------------------------------------------

-- | Provides an implementation of the Thrift Binary Protocol.
binaryProtocol :: Protocol
binaryProtocol = Protocol putMessage putTValue getMessage getTValue

versionMask :: Word32
versionMask = 0xffff0000

version1 :: Word32
version1 = 0x80010000

getLength :: Get Int
getLength = fromIntegral <$> getInt32le

putLength :: Int -> Put
putLength = putInt32be . fromIntegral

getTypeCode :: Get TypeCode
getTypeCode = getInt8

putTypeCode :: TypeCode -> Put
putTypeCode = putInt8

------------------------------------------------------------------------------

putMessage :: Message -> Put
putMessage msg = do
    putWord32be (version1 .|. fromIntegral (messageType msg))
    putByteString (TE.encodeUtf8 $ messageName msg)
    putInt32be (messageId msg)
    putTValue (messagePayload msg)

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
        (`HM.traverseWithKey` a) $ \ fid val -> do
            putInt8 (toTypeCode val)
            putInt16be fid
            putTValue val
        putTypeCode stopTypeCode

    (TList t a) -> do
        putTypeCode t
        putLength (length a)
        mapM_ putTValue a

    (TMap tk tv a) -> do
        putTypeCode tk
        putTypeCode tv
        putLength (HM.size a)
        forM_ (HM.toList a) $ \ (key, val) -> do
            putTValue key
            putTValue val

    (TSet t a) -> do
        putTypeCode t
        putLength (HS.size a)
        mapM_ putTValue (HS.toList a)

------------------------------------------------------------------------------

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
            tcode = fromIntegral (0x000000ff .&. versionAndType)
        unless (version /= version1) (fail $ "Unsupported version: " ++ show version)
        nlen <- getInt32be
        Message <$> (TE.decodeUtf8 <$> getByteString (fromIntegral nlen))
                <*> pure tcode
                <*> getInt32be
                <*> getTValue typ

    -- name~4 type:1 seqid:4 payload
    parseNonStrict nlen =
        Message <$> (TE.decodeUtf8 <$> getByteString (fromIntegral nlen))
                <*> getInt8
                <*> getInt32be
                <*> getTValue typ

getTValue :: TypeCode -> Get TValue
getTValue t =
    if  | t == tBoolTypeCode   -> TBool . (/=0) <$> getWord8
        | t == tInt8TypeCode   -> TInt8 <$> getInt8
        | t == tDoubleTypeCode -> TDouble <$> getDoublebe
        | t == tInt16TypeCode  -> TInt16 <$> getInt16be
        | t == tInt32TypeCode  -> TInt32 <$> getInt32be
        | t == tInt64TypeCode  -> TInt64 <$> getInt64be
        | t == tBinaryTypeCode -> TBinary <$> (getByteString =<< getLength)
        | t == tStructTypeCode -> TStruct . HM.fromList <$> go
        | t == tMapTypeCode    -> do kt <- getTypeCode
                                     vt <- getTypeCode
                                     len <- getLength
                                     TMap kt vt . HM.fromList <$>
                                        replicateM len ((,) <$> getTValue kt <*> getTValue vt)
        | t == tSetTypeCode    -> do vt <- getTypeCode
                                     len <- getLength
                                     TSet vt . HS.fromMap . HM.fromList <$>
                                        replicateM len ((,) <$> getTValue vt <*> pure ())
        | t == tListTypeCode   -> do vt <- getTypeCode
                                     len <- getLength
                                     TList vt <$> replicateM len (getTValue vt)

  where
    go :: Get [(Int16, TValue)]
    go = do
        typ <- getTypeCode
        if typ == stopTypeCode
        then return []
        else do
            fid <- getInt16be
            val <- getTValue typ
            rest <- go
            fid `seq` val `seq` return ((fid, val) : rest)

