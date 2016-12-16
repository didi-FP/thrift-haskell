module Thrift.Transport.TCP where

import Network.BSD (HostName, PortNumber)
import System.IO.Streams.TCP
import Thrift.Type

openTransport :: HostName
              -> PortNumber
              -> IO Transport
openTransport h p = do
    (i, o, s) <- connect h p
    return (Transport i o (close s))



