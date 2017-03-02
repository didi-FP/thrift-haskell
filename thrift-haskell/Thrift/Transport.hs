module Thrift.Transport where

import Network.BSD (HostName, PortNumber)
import Data.Binary.Put (putInt32be)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.Lazy.Internal as BL
import Data.Bits ((.|.), (.&.), shiftR, shiftL)
import Data.Connection
import qualified System.IO.Streams.TCP as TCP
import qualified System.IO.Streams as Streams
import Thrift.Type
import Data.Word

openTcpTransport :: HostName
              -> PortNumber
              -> IO Transport
openTcpTransport h p = do
    (Connection src send cls _) <- TCP.connect h p
    return (Connection src send cls ())

framed :: Transport -> IO Transport
framed (Connection src send cls _) = do
    siz <- fromIntegral . bsToWord32be <$> Streams.readExactly 4 src  -- frame size
    src' <- Streams.takeExactly siz src
    return (Connection src' send' cls ())
  where
    send' lbs = do
        let siz = fromIntegral $ BL.length lbs
        send (BL.Chunk (word32beToBS siz) lbs)

    word32beToBS :: Word32 -> B.ByteString
    word32beToBS w = B.pack [ (fromIntegral $ w `shiftR` 24) .&. 0xFF
                            , (fromIntegral $ w `shiftR` 16) .&. 0xFF
                            , (fromIntegral $ w `shiftR` 8) .&. 0xFF
                            , (fromIntegral $ w) .&. 0xFF
                            ]

    bsToWord32be :: B.ByteString -> Word32
    bsToWord32be s =
          (fromIntegral (s `B.unsafeIndex` 0) `shiftL` 24) .|.
          (fromIntegral (s `B.unsafeIndex` 1) `shiftL` 16) .|.
          (fromIntegral (s `B.unsafeIndex` 2) `shiftL`  8) .|.
          (fromIntegral (s `B.unsafeIndex` 3) )
