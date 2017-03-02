{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Thrift.Type
  ( -- * Thrift value and types
    TypeCode
  , pattern TC_Stop
  , pattern TC_Void
  , pattern TC_Bool
  , pattern TC_Int8
  , pattern TC_Double
  , pattern TC_Int16
  , pattern TC_Int32
  , pattern TC_Int64
  , pattern TC_Binary
  , pattern TC_Struct
  , pattern TC_Map
  , pattern TC_Set
  , pattern TC_List
  , TValue(..)
  , tValueTC
  , TypeCodeTagged(..)
  , Thrift(..)
  -- * Message
  , MessageType
  , pattern MT_Call
  , pattern MT_Reply
  , pattern MT_Exception
  , pattern MT_Oneway
  , Message(..)
  -- * Protocol and Transport
  , Protocol(..)
  , Transport(..)
  , AppException(..)
  -- * utilities used in compiled IDL
  , lookupOptional
  , lookupRequired
  , lookupDefault
  , IM.lookup
  , request
  , T.Text
  , B.ByteString
  , HM.HashMap
  , HS.HashSet
  , Int8, Int16, Int32, Int64
  , mkIntMap
  , mkHashMap
  , Data(..)
  , Typeable(..)
  , Generic(..)
  , Hashable(..)
  ) where

import Data.Int
import Data.Connection
import Data.IORef (newIORef, atomicModifyIORef', IORef)
import Control.Monad (when, unless)
import Control.Exception (Exception(..), throwIO)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Binary.Get (Get)
import Data.Binary.Put (Put, runPut)
import Data.Hashable (Hashable(..))
import qualified Data.IntMap as IM
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Data (Data(..))
import Data.Typeable (Typeable(..), Proxy(..))
import GHC.Generics (Generic(..))
import Data.Hashable (Hashable(..))
import System.IO.Streams (InputStream, OutputStream)
import System.IO.Streams.Binary (getFromStream)
import System.IO.Streams.ByteString (writeLazyByteString)
import System.IO.Unsafe (unsafePerformIO)

type TypeCode = Int8

-- | Singleton type to present all thrift values.
--
data TValue
    = TBool   !Bool
    | TInt8   !Int8
    | TInt16  !Int16
    | TInt32  !Int32
    | TInt64  !Int64
    | TDouble !Double
    | TBinary !B.ByteString
    | TStruct [(Int, TValue)]
    | TMap    !TypeCode !TypeCode [(TValue, TValue)]
    | TSet    !TypeCode [TValue]
    | TList   !TypeCode [TValue]
  deriving (Eq, Show, Data, Typeable, Generic)

instance Hashable TValue where
    hashWithSalt s a = case a of
        TBool    x -> s `hashWithSalt` (0 :: Int) `hashWithSalt` x
        TInt8    x -> s `hashWithSalt` (1 :: Int) `hashWithSalt` x
        TDouble  x -> s `hashWithSalt` (2 :: Int) `hashWithSalt` x
        TInt16   x -> s `hashWithSalt` (3 :: Int) `hashWithSalt` x
        TInt32   x -> s `hashWithSalt` (4 :: Int) `hashWithSalt` x
        TInt64   x -> s `hashWithSalt` (5 :: Int) `hashWithSalt` x
        TBinary  x -> s `hashWithSalt` (6 :: Int) `hashWithSalt` x
        TStruct  x -> s `hashWithSalt` (7 :: Int) `hashWithSalt` x
        TMap x y z -> s `hashWithSalt` (8 :: Int) `hashWithSalt` x `hashWithSalt` y `hashWithSalt` z
        TSet x y   -> s `hashWithSalt` (8 :: Int) `hashWithSalt` x `hashWithSalt` y
        TList x y  -> s `hashWithSalt` (9 :: Int) `hashWithSalt` x `hashWithSalt` y

pattern TC_Stop   = 0  :: TypeCode
pattern TC_Void   = 1  :: TypeCode
pattern TC_Bool   = 2  :: TypeCode
pattern TC_Int8   = 3  :: TypeCode
pattern TC_Double = 4  :: TypeCode
pattern TC_Int16  = 6  :: TypeCode
pattern TC_Int32  = 8  :: TypeCode
pattern TC_Int64  = 10 :: TypeCode
pattern TC_Binary = 11 :: TypeCode
pattern TC_Struct = 12 :: TypeCode
pattern TC_Map    = 13 :: TypeCode
pattern TC_Set    = 14 :: TypeCode
pattern TC_List   = 15 :: TypeCode

-- | Map a 'TValue' to its type code.
tValueTC :: TValue -> TypeCode
tValueTC (TBool    _)  = TC_Bool
tValueTC (TInt8    _)  = TC_Int8
tValueTC (TDouble  _)  = TC_Double
tValueTC (TInt16   _)  = TC_Int16
tValueTC (TInt32   _)  = TC_Int32
tValueTC (TInt64   _)  = TC_Int64
tValueTC (TBinary  _)  = TC_Binary
tValueTC (TStruct  _)  = TC_Struct
tValueTC (TMap _ _ _)  = TC_Map
tValueTC (TSet   _ _)  = TC_Set
tValueTC (TList  _ _)  = TC_List
{-# INLINABLE tValueTC #-}

-- | TypeCode tagged with value type, This is useful in empty `HM.HashMap` case:
-- even if we don't have any concrete 'TValue', we still can get k & v 's type code.
--
newtype TypeCodeTagged a = TypeCodeTagged { getTypeCode :: TypeCode }

-- | The main class
class Thrift a where
    typeCode :: TypeCodeTagged a
    defaultValue :: a
    toTValue :: a -> TValue
    fromTValue :: TValue -> a

instance Thrift Bool where
    typeCode = TypeCodeTagged TC_Bool
    defaultValue = False
    toTValue i = TBool i
    fromTValue (TBool i) = i
    fromTValue _         = error "bad bool"

instance Thrift Int8 where
    typeCode = TypeCodeTagged TC_Int8
    defaultValue = 0
    toTValue i = TInt8 i
    fromTValue (TInt8 i) = i
    fromTValue _         = error "bad int8"

instance Thrift Double where
    typeCode = TypeCodeTagged TC_Double
    defaultValue = 0
    toTValue i = TDouble i
    fromTValue (TDouble i) = i
    fromTValue _         = error "bad double"

instance Thrift Int16 where
    typeCode = TypeCodeTagged TC_Int16
    defaultValue = 0
    toTValue i = TInt16 i
    fromTValue (TInt16 i) = i
    fromTValue _         = error "bad int16"

instance Thrift Int32 where
    typeCode = TypeCodeTagged TC_Int32
    defaultValue = 0
    toTValue i = TInt32 i
    fromTValue (TInt32 i) = i
    fromTValue _         = error "bad int32"

instance Thrift Int64 where
    typeCode = TypeCodeTagged TC_Int64
    defaultValue = 0
    toTValue i = TInt64 i
    fromTValue (TInt64 i) = i
    fromTValue _         = error "bad int64"

instance Thrift T.Text where
    typeCode = TypeCodeTagged TC_Binary
    defaultValue = T.empty
    toTValue t = TBinary (T.encodeUtf8 t)
    fromTValue (TBinary b) = (T.decodeUtf8 b)
    fromTValue _         = error "bad string"

instance Thrift B.ByteString where
    typeCode = TypeCodeTagged TC_Binary
    defaultValue = B.empty
    toTValue b = TBinary b
    fromTValue (TBinary b) = b
    fromTValue _         = error "bad binary"

instance (Thrift k, Thrift v, Eq k, Eq v, Hashable k) => Thrift (HM.HashMap k v) where
    typeCode = TypeCodeTagged TC_Map
    defaultValue = HM.empty
    toTValue m = TMap (getTypeCode (typeCode :: TypeCodeTagged k))
                    (getTypeCode (typeCode :: TypeCodeTagged v)) $
                    map (\ (k, v) -> (toTValue k, toTValue v)) (HM.toList m)
    fromTValue (TMap _ _ kvs) = HM.fromList $ map (\ (k, v) -> (fromTValue k, fromTValue v)) kvs
    fromTValue _         = error "bad map"

instance (Thrift v, Eq v, Hashable v) => Thrift (HS.HashSet v) where
    typeCode = TypeCodeTagged TC_Set
    defaultValue = HS.empty
    toTValue s = TSet (getTypeCode (typeCode :: TypeCodeTagged v)) $
                    map toTValue (HS.toList s)
    fromTValue (TSet _ xs) = HS.fromList $ map fromTValue xs
    fromTValue _         = error "bad set"

instance (Thrift v) => Thrift [v] where
    typeCode = TypeCodeTagged TC_List
    defaultValue = []
    toTValue xs = TList (getTypeCode (typeCode :: TypeCodeTagged v)) $
                    map toTValue xs
    fromTValue (TList _ xs) = map fromTValue xs
    fromTValue _         = error "bad list"

instance Thrift () where
    typeCode = TypeCodeTagged TC_Struct
    defaultValue = ()
    toTValue _ = TStruct []
    fromTValue (TStruct []) = ()
    fromTValue _ = error "bad void"

lookupOptional :: (Thrift a) => Maybe a -> Int -> IM.IntMap TValue -> Maybe a
lookupOptional def key m = case IM.lookup key m of
    Nothing -> def
    Just a  -> Just (fromTValue a)

lookupRequired :: Thrift a => Int -> IM.IntMap TValue -> a
lookupRequired key m = case IM.lookup key m of
    Nothing -> error $ "can't find field identifier: " ++ show key
    Just a  -> fromTValue a

lookupDefault :: (Thrift a) => a -> Int -> IM.IntMap TValue -> a
lookupDefault def key m = case IM.lookup key m of
    Nothing -> def
    Just a  -> fromTValue a

mkIntMap :: [(Int, TValue)] -> IM.IntMap TValue
mkIntMap = IM.fromList

mkHashMap :: (Eq k, Hashable k, Hashable v) => [(k, v)] -> HM.HashMap k v
mkHashMap = HM.fromList

--------------------------------------------------------------------------------

-- | Type of message being sent.
--
type MessageType = Int8

pattern MT_Call       = 1 :: MessageType
pattern MT_Reply      = 2 :: MessageType
pattern MT_Exception  = 3 :: MessageType
pattern MT_Oneway     = 4 :: MessageType

-- | Message envelope for Thrift payloads.
data Message = Message
    { messageName    :: !T.Text
    , messageType    :: !MessageType
    , messageId      :: !Int32
    , messagePayload :: !TValue
    }
  deriving (Show, Eq)

-- | Protocols define a specific way to convert values into binary and back.
data Protocol = Protocol
    { encodeMessage :: Message -> Put
    , encodeTValue  :: TValue -> Put
    , decodeMessage :: TypeCode -> Get Message
    , decodeTValue  :: Int8 -> Get TValue
    }

type Transport = Connection ()

seqIdGen :: IORef Int32
seqIdGen = unsafePerformIO (newIORef 0)
{-# NOINLINE seqIdGen #-}

seqId :: IO Int32
seqId = atomicModifyIORef' seqIdGen $ \x -> let z = x+1 in (z, z)
{-# INLINABLE seqId #-}

-- | polymorphric request method
--
request :: forall req res . (Thrift req, Thrift res)
        => T.Text  -- method name
        -> Bool  -- is oneway
        -> Protocol
        -> Transport
        -> req -> IO res
request rpcName rpcOneWay Protocol{..} conn req = do
    sid <- seqId
    -- serialize request
    let msg = Message rpcName
                (if rpcOneWay then MT_Oneway else MT_Call)
                sid
                (toTValue req)
    send conn (runPut (encodeMessage msg))
    -- deserialize respond
    if rpcOneWay
    then return defaultValue
    else do
        res <- getFromStream (decodeMessage $ getTypeCode (typeCode :: TypeCodeTagged res))
                             (source conn)
        case res of
            Just Message{..} -> do
                when (messageId /= sid) . throwIO $
                    AppException AE_BAD_SEQUENCE_ID "client sequence id verify failed"
                case messageType of
                    MT_Exception -> throwIO (fromTValue messagePayload :: AppException)
                    MT_Reply -> return (fromTValue messagePayload)
                    _ -> throwIO $ AppException
                                    AE_INVALID_MESSAGE_TYPE
                                    ("unknown message type: " `T.append`
                                        (T.pack $ show messageType))

            Nothing -> throwIO $ AppException AE_INTERNAL_ERROR "bad network"

respond :: (Thrift req, Thrift res)
        => Protocol
        -> Transport
        -> (req -> IO res)
        -> IO ()
respond = error "WIP"

--------------------------------------------------------------------------------

-- | The application level exception, it's not intend to be handled by programmer.
type AppExType = Int32

pattern AE_UNKNOWN                    = 0
pattern AE_UNKNOWN_METHOD             = 1
pattern AE_INVALID_MESSAGE_TYPE       = 2
pattern AE_WRONG_METHOD_NAME          = 3
pattern AE_BAD_SEQUENCE_ID            = 4
pattern AE_MISSING_RESULT             = 5
pattern AE_INTERNAL_ERROR             = 6
pattern AE_PROTOCOL_ERROR             = 7
pattern AE_INVALID_TRANSFORM          = 8
pattern AE_INVALID_PROTOCOL           = 9
pattern AE_UNSUPPORTED_CLIENT_TYPE    = 10

data AppException = AppException
    { appExceptionType :: AppExType
    , appExceptionMessage :: T.Text
    } deriving (Eq, Show, Typeable)

instance Exception AppException
instance Thrift AppException where
    typeCode = TypeCodeTagged TC_Struct
    defaultValue = AppException AE_UNKNOWN ""
    toTValue (AppException typ msg) =
        TStruct [ (1, toTValue msg) , (2, toTValue typ) ]
    fromTValue (TStruct xs) =
        let m = IM.fromList xs
            msg = lookupRequired 1 m
            typ = lookupRequired 2 m
        in AppException msg typ
    fromTValue _ = error "bad exception struct"
