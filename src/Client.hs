module Client where

import qualified Control.Exception as E
import qualified Data.ByteString as BS 
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

type NumBytes = Int

sendBytes :: HostName -> ServiceName -> BS.ByteString -> NumBytes -> IO BS.ByteString 
sendBytes h s b n =
  withSocketsDo $ do
    addr <- resolve h s
    E.bracket (open addr) close talk
  where
    resolve host port = do
      let hints = defaultHints {addrSocketType = Datagram}
      addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      return sock
    talk sock = do
      sendAll sock b
      recv sock n
     
-- Include a timeout for recv.
