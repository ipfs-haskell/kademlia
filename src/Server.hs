module Server where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import qualified Data.ByteString as S
import qualified Network.Datagram as D
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Utils

type NumConn = Int

type NumBytes = Int

-- Need to handle exceptions
runServer port = do
  let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Datagram}
  addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  D.runUdpT1024 (serverComputation addr) sock

serverComputation addr
  -- sock <- D.UdpT ask
  -- liftIO $ setSocketOption sock ReuseAddr 1
  -- -- If the prefork technique is not used,
  -- -- set CloseOnExec for the security reasons.
  -- fd <- liftIO $ fdSocket sock
  -- liftIO $ setCloseOnExecIfNeeded fd
 = do
  D.bind $ addrAddress addr
  forever $ do
    (dgram, addr) <- D.receive
    liftIO $ printRecvDatagram (dgram, addr)
    D.send addr dgram
