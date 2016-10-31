-- |
-- Module      :  Pinch.Protocol
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- Protocols in Pinch only need to know how to serialize and deserialize
-- 'Value' objects. Types that want to be serialized into/from Thrift payloads
-- define how to convert them to/from 'Value' objects via
-- 'Pinch.Pinchable.Pinchable'.
module Data.Thrift.Protocol
    ( Protocol(..)
    ) where

import Data.Thrift.Type
import Data.Thrift.Protocol.Binary
