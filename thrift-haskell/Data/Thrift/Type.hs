{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Thrift.Type where

import Data.Int
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Binary.Get (Get)
import Data.Binary.Put (Put)
import Data.Hashable (Hashable(..))

type TypeCode = Int8

pattern TC_Stop :: TypeCode
pattern TC_Stop      = 0
pattern TC_Void :: TypeCode
pattern TC_Void      = 1
pattern TC_Bool :: TypeCode
pattern TC_Bool     = 2
pattern TC_Int8 :: TypeCode
pattern TC_Int8     = 3
pattern TC_Double :: TypeCode
pattern TC_Double   = 4
pattern TC_Int16 :: TypeCode
pattern TC_Int16    = 6
pattern TC_Int32 :: TypeCode
pattern TC_Int32    = 8
pattern TC_Int64 :: TypeCode
pattern TC_Int64    = 10
pattern TC_Binary :: TypeCode
pattern TC_Binary   = 11
pattern TC_Struct :: TypeCode
pattern TC_Struct   = 12
pattern TC_Map :: TypeCode
pattern TC_Map      = 13
pattern TC_Set :: TypeCode
pattern TC_Set      = 14
pattern TC_List :: TypeCode
pattern TC_List     = 15

data TValue
    = TBool   !Bool
    | TInt8   !Int8
    | TDouble !Double
    | TInt16  !Int16
    | TInt32  !Int32
    | TInt64  !Int64
    | TBinary !ByteString
    | TStruct !(HM.HashMap Int16 TValue)
    | TMap    !TypeCode !TypeCode !(HM.HashMap TValue TValue)
    | TSet    !TypeCode !(HS.HashSet TValue)
    | TList   !TypeCode ![TValue]
  deriving (Eq, Show)

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


-- | Map a 'TValue' to its type code.
toTypeCode :: TValue -> TypeCode
toTypeCode (TBool    _)  = TC_Bool
toTypeCode (TInt8    _)  = TC_Int8
toTypeCode (TDouble  _)  = TC_Double
toTypeCode (TInt16   _)  = TC_Int16
toTypeCode (TInt32   _)  = TC_Int32
toTypeCode (TInt64   _)  = TC_Int64
toTypeCode (TBinary  _)  = TC_Binary
toTypeCode (TStruct  _)  = TC_Struct
toTypeCode (TMap _ _ _)  = TC_Map
toTypeCode (TSet   _ _)  = TC_Set
toTypeCode (TList  _ _)  = TC_List
{-# INLINE toTypeCode #-}


-- | Type of message being sent.
type MessageType = Int8

pattern MT_Call :: MessageType
pattern MT_Call = 1
pattern MT_Reply :: MessageType
pattern MT_Reply = 2
pattern MT_Exception :: MessageType
pattern MT_Exception = 3
pattern MT_Oneway :: MessageType
pattern MT_Oneway = 4

-- | Message envelope for Thrift payloads.
data Message = Message
    { messageName    :: !Text
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
    , decodeTValue  :: TypeCode -> Get TValue
    }
