{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Thrift.Arbitrary where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as S
import Thrift.Type

halfSize :: Gen a -> Gen a
halfSize = scale (\n -> truncate (fromIntegral n / 2 :: Double))

genTC :: Gen TypeCode
genTC = elements [ TC_Bool
                 , TC_Int8
                 , TC_Double
                 , TC_Int16
                 , TC_Int32
                 , TC_Int64
                 , TC_Binary
                 , TC_Struct
                 , TC_Map
                 , TC_Set
                 , TC_List ] -- no TC_Void, TC_Stop

genMT :: Gen MessageType
genMT = elements [ MT_Call, MT_Reply, MT_Oneway ] -- no MT_Exception

instance Arbitrary TValue where
    arbitrary = genTC >>= genTValue

      where
        genTValue tc = case tc of
            TC_Bool -> TBool <$> arbitrary
            TC_Int8 -> TInt8 <$> arbitrary
            TC_Double -> TDouble <$> arbitrary
            TC_Int16 -> TInt16 <$> arbitrary
            TC_Int32 -> TInt32 <$> arbitrary
            TC_Int64 -> TInt64 <$> arbitrary
            TC_Binary -> TBinary . getSomeByteString <$> arbitrary
            TC_Struct -> genStruct
            TC_Map -> genMap
            TC_Set -> genSet
            TC_List -> genList

        genStruct = halfSize $ TStruct <$> listOf genField
          where
            genField = (,) <$> (getPositive <$> arbitrary)
                           <*> arbitrary

        genMap = do
            ktc <- genTC
            vtc <- genTC
            halfSize $ TMap ktc vtc <$>
                listOf ((,) <$> genTValue ktc <*> genTValue vtc)

        genSet = do
            tc <- genTC
            halfSize $ TSet tc <$> (listOf $ genTValue tc)

        genList = do
            tc <- genTC
            halfSize $ TList tc <$> (listOf $ genTValue tc)

    shrink x = case x of
        TInt8 x   -> TInt8 <$> shrink x
        TDouble x -> TDouble <$> shrink x
        TInt16 x  -> TInt16 <$> shrink x
        TInt32 x  -> TInt32 <$> shrink x
        TInt64 x  -> TInt64 <$> shrink x
        TBinary x -> TBinary . getSomeByteString <$> shrink (SomeByteString x)

        TStruct xs -> TStruct <$> shrink xs
        TMap ktc vtc xs -> TMap ktc vtc <$> shrink xs
        TSet tc xs -> TSet tc <$> shrink xs
        TList tc xs -> TList tc <$> shrink xs

instance Arbitrary Message where
    arbitrary = Message <$> (getSomeText <$> arbitrary)
                        <*> genMT
                        <*> arbitrary
                        <*> arbitrary

    shrink (Message name  typ  mid  body) =
        [   Message name' typ' mid' body'
        | ((SomeText name'), typ', mid', body') <-
            shrink ((SomeText name), typ, mid, body)
        ]

--------------------------------------------------------------------------------

newtype SomeByteString = SomeByteString
    { getSomeByteString :: ByteString }
  deriving (Show, Eq)

instance Arbitrary SomeByteString where
    arbitrary = SomeByteString . B.pack <$> arbitrary

    shrink (SomeByteString bs)
        | B.null bs = []
        | otherwise =
            SomeByteString . B.pack <$> shrink (B.unpack bs)

newtype SomeText = SomeText { getSomeText :: Text }

instance Arbitrary SomeText where
    arbitrary = SomeText . T.pack <$> arbitrary
    shrink = map (SomeText . T.pack) . shrink . T.unpack . getSomeText
