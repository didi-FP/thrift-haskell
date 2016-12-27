module Thrift.Transport where

import Network.BSD (HostName, PortNumber)
import qualified Data.ByteString.Unsafe as B
import System.IO.Streams.TCP
import qualified System.IO.Streams as Streams
import Thrift.Type

openTcpTransport :: HostName
              -> PortNumber
              -> IO Transport
openTcpTransport h p = do
    (i, o, s) <- connect h p
    return (Transport i o (close s))


