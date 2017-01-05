{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      :  Thrift.Protocol.Binary
-- Copyright   :  (c) Winterland 2016
-- License     :  BSD3
--
-- Maintainer  :  Winterland <drkoster@qq.com>
-- Stability   :  experimental
--
-- Thrift Compact Protocol.
-- Implements the Thrift Compact Protocol as a 'Protocol'.
--
-- Spec: <https://github.com/apache/thrift/blob/master/doc/specs/thrift-compact-protocol.md>
--
module Thrift.Protocol.Compact where

import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Data.Bits
import Data.ByteString                  (ByteString)
import Data.Int
import Data.Word

import qualified Data.ByteString     as B
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.Text.Encoding  as TE
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.IEEE754

import Thrift.Type

------------------------------------------------------------------------------

int32ToZigZag :: Int32 -> Word32
int32ToZigZag n = fromIntegral $ xor (shiftL n 1) (shiftR n 31)

{-# INLINE int32ToZigZag #-}

zigZagToInt32 :: Word32 -> Int32
zigZagToInt32 n = fromIntegral $ xor (shiftR n 1)  (- (n .&. 1))

{-# INLINE zigZagToInt32 #-}

int64ToZigZag :: Int64 -> Word64
int64ToZigZag n = fromIntegral $ xor (shiftL n 1) (shiftR n 63)

{-# INLINE int64ToZigZag #-}

zigZagToInt64 :: Word64 -> Int64
zigZagToInt64 n = fromIntegral $ xor (shiftR n 1)  (- (n .&. 1))

{-# INLINE zigZagToInt64 #-}

putVarWord64le :: Word64 -> Put
putVarWord64le n
    | n < 128   = putWord8 (fromIntegral n)
    | otherwise = do putWord8 (fromIntegral n .&. 0x7f .|. 0x80)
                     putVarWord64le (shiftR n 7)

{-# INLINE putVarWord64le #-}

putVarInt16le :: Int16 -> Put
putVarInt16le = putVarWord64le . fromIntegral . int32ToZigZag . fromIntegral

{-# INLINE putVarInt16le #-}

putVarInt32le :: Int32 -> Put
putVarInt32le = putVarWord64le . fromIntegral . int32ToZigZag

{-# INLINE putVarInt32le #-}

putVarInt64le :: Int64 -> Put
putVarInt64le = putVarWord64le . int64ToZigZag

{-# INLINE putVarInt64le #-}

getVarWord64le :: Get Word64
getVarWord64le = go 0 0
  where
    go !s !n = do
        b <- getWord8
        let n' = n + shiftL (fromIntegral (b .&. 0x7f)) s
        if (b .&. 0x80) == 0
           then return n'
           else go (s+7) n'

{-# INLINE getVarWord64le #-}

getVarInt16le :: Get Int16
getVarInt16le = (fromIntegral . zigZagToInt32 . fromIntegral) <$> getVarWord64le

{-# INLINE getVarInt16le #-}

getVarInt32le :: Get Int32
getVarInt32le = (zigZagToInt32 . fromIntegral) <$> getVarWord64le

{-# INLINE getVarInt32le #-}

getVarInt64le :: Get Int64
getVarInt64le = zigZagToInt64 <$> getVarWord64le

{-# INLINE getVarInt64le #-}

------------------------------------------------------------------------------

-- | Provides an implementation of the Thrift Compact Protocol.
compactProtocol :: Protocol
compactProtocol = Protocol putMessage putTValue getMessage getTValue

protocolId :: Word8
protocolId = 0x82

versionMask :: Word8
versionMask = 0x1f

version1 :: Word8
version1 = 0x01

getLength :: Get Int
getLength = fromIntegral <$> getVarWord64le

putLength :: Int -> Put
putLength = putVarWord64le . fromIntegral

------------------------------------------------------------------------------

putMessage :: Message -> Put
putMessage Message{..} = do
    putWord8 protocolId
    putWord8 (shiftL (fromIntegral messageType) 5 .|. version1)
    putVarWord64le (fromIntegral messageId)
    let name = TE.encodeUtf8 messageName
    putLength (B.length name)
    putByteString name
    putTValue messagePayload

getMessage :: TypeCode -> Get Message
getMessage typ = do
    protocol <- getWord8
    when (protocol /= protocolId) (fail $ "Unsupported protocol: " ++ show protocol)
    versionAndType <- getWord8
    let version = versionMask .&. versionAndType
        tcode = fromIntegral (shiftR versionAndType 5)
    when (version /= version1) (fail $ "Unsupported version: " ++ show version)
    seqId <- fromIntegral <$> getVarWord64le
    nlen <- getLength
    Message <$> (TE.decodeUtf8 <$> getByteString nlen)
            <*> pure tcode
            <*> pure seqId
            <*> getTValue typ

------------------------------------------------------------------------------

-- | Reference: https://github.com/apache/thrift/blob/master/lib/hs/src/Thrift/Protocol/Compact.hs#L253-L301

-- | Convert ordinary type code to type code in struct's filed in
-- compact protocol.
toCompactType :: TypeCode -> TypeCode
toCompactType 0 = 0  -- stop field
toCompactType 1 = 1  -- void type
toCompactType 2 = 1  -- boolean, used for collections, list<bool> or set<bool>
toCompactType 3 = 3
toCompactType 6 = 4
toCompactType 8 = 5
toCompactType 10 = 6
toCompactType 4 = 7
toCompactType 11 = 8
toCompactType 15 = 9
toCompactType 14 = 10
toCompactType 13 = 11
toCompactType 12 = 12

-- | Convert  type code in struct's filed to ordinary type code in
-- compact protocol.
fromCompactType :: TypeCode -> Get TypeCode
fromCompactType 0 = return 0  -- stop field
fromCompactType 1 = return 2  -- boolean true
fromCompactType 2 = return 2  -- boolean false
fromCompactType 3 = return 3
fromCompactType 4 = return 6
fromCompactType 5 = return 8
fromCompactType 6 = return 10
fromCompactType 7 = return 4
fromCompactType 8 = return 11
fromCompactType 9 = return 15
fromCompactType 10 = return 14
fromCompactType 11 = return 13
fromCompactType 12 = return 12
fromCompactType t = fail $ "Unknown compact field type code: " ++ show t

------------------------------------------------------------------------------

putTValue :: TValue -> Put
putTValue (TBinary a) = do putLength (B.length a)
                           putByteString a
putTValue (TBool   a) = putInt8 (if a then 0x01 else 0x02)  -- 1: true, 2: false, use the same encoding as struct's fields.
putTValue (TInt8   a) = putInt8 a
putTValue (TDouble a) = putDoublebe a
putTValue (TInt16  a) = putVarInt16le a
putTValue (TInt32  a) = putVarInt32le a
putTValue (TInt64  a) = putVarInt64le a
putTValue (TStruct a) = do
    go (minBound :: Int) a
    putInt8 TC_Stop
  where
    go :: Int -> [(Int, TValue)] -> Put
    go _ [] = return ()
    go previous ((fid,val):xs) = do
        let delta = fid - previous
            tcode = case val of
                   TBool True  -> 1
                   TBool False -> 2
                   _           -> fromIntegral (toCompactType (tValueTC val))
        if delta <= 15
           then putWord8 (shiftL (fromIntegral delta) 4 .|. tcode)
           else putWord8 tcode >> putVarInt16le (fromIntegral fid)
        case val of
            TBool _ -> return ()
            _       -> putTValue val
        go fid xs
putTValue (TList t a) = do
    let nlen = length a
    if nlen < 15
       then putWord8 (shiftL (fromIntegral nlen) 4 .|. fromIntegral (toCompactType t))
       else putWord8 (fromIntegral (toCompactType t)) >> putLength nlen
    mapM_ putTValue a
putTValue (TMap tk tv a) = do
    let nlen = length a
    if nlen == 0
       then putInt8 0x00
       else do
           putLength nlen
           putWord8 (shiftL (fromIntegral (toCompactType tk)) 4 .|. fromIntegral (toCompactType tv))
           forM_ a $ \ (key, val) -> putTValue key >> putTValue val
putTValue (TSet t a) = putTValue $ TList t a

getTValue :: TypeCode -> Get TValue
getTValue TC_Binary = TBinary <$> (getByteString =<< getLength)
getTValue TC_Bool   = TBool . (/=2) <$> getWord8  -- 1: true, 2: false, use the same encoding as struct's fields.
getTValue TC_Int8   = TInt8 <$> getInt8
getTValue TC_Double = TDouble <$> getDoublebe
getTValue TC_Int16  = TInt16 <$> getVarInt16le
getTValue TC_Int32  = TInt32 <$> getVarInt32le
getTValue TC_Int64  = TInt64 <$> getVarInt64le
getTValue TC_Struct = TStruct <$> go 0  -- the extra argument means the previous field id.
  where
    go :: Int -> Get [(Int, TValue)]
    go previous = do
        fieldAndType <- getWord8
        let delta = fromIntegral $ shiftR fieldAndType 4
        tcode <- fromCompactType . fromIntegral $ 0x0f .&. fieldAndType
        if tcode == TC_Stop
            then if delta == 0
                   then return []
                   else fail $ "Unsupported field header: " ++ show fieldAndType
            else do
                fid <- if (0xf0 .&. fieldAndType) == 0
                          then fromIntegral <$> getVarInt16le
                          else return (previous + delta)
                val <- case 0x0f .&. fieldAndType of
                         1 -> return $ TBool True
                         2 -> return $ TBool False
                         _ -> getTValue (fromIntegral tcode)
                rest <- go fid
                fid `seq` val `seq` return ((fid, val) : rest)

getTValue TC_List   = do
    nlenAndType <- getWord8
    tcode <- fromCompactType . fromIntegral $ 0x0f .&. nlenAndType
    nlen <- if (0xf0 .&. nlenAndType) == 0xf0
               then fromIntegral <$> getLength
               else return $ fromIntegral (shiftR nlenAndType 4)
    TList tcode <$> replicateM nlen (getTValue tcode)
getTValue TC_Map    = do
    nlen <- fromIntegral <$> getLength
    if nlen == 0
        then return $ TMap 1 1 [] -- use void type as typecode.
        else do
            kv <- getWord8
            kt <- fromCompactType . fromIntegral $ shiftR kv 4
            vt <- fromCompactType . fromIntegral $ 0x0f .&. kv
            TMap kt vt <$>
                replicateM nlen ((,) <$> getTValue kt <*> getTValue vt)
getTValue TC_Set    = (\(TList vt elems) -> TSet vt elems) <$> getTValue TC_List
getTValue tc        = fail $ "Bad type code: " ++ show tc
