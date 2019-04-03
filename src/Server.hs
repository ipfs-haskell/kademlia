module Server where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import qualified Data.ByteString as S
import qualified Network.Datagram as D
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Utils
import Data.Serialize as Se
import Protocol
import Spec
import Network.Datagram

type NumConn = Int

type NumBytes = Int

-- Need to handle exceptions
-- runServer port = do
--   let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Datagram}
--   addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
--   sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
--   D.runUdpT1024 (serverComputation addr) sock

-- ReaderT StateT UdpT
serverComputation addr
 = do
  lift . lift . D.bind . addrAddress $ addr
  forever $ do
    (dgram, addr) <- lift . lift $ D.receive
    let erpc = Se.decode . unwrap $ dgram :: Either String RPC
    liftIO $ print erpc
    case erpc of
      Left err -> liftIO $ print err
      Right rpc -> answer rpc >>= lift . lift . D.send addr . D.unsafeDatagram . Se.encode
