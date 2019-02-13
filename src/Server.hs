module Server where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, unless, void)
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Network.Datagram as D
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader

type NumConn = Int

type NumBytes = Int

runServer s m n port = do
    let hints =
          defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Datagram}
    addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    D.runUdpT1024 (c addr) sock
    where
      c addr = do
        sock <- D.UdpT ask
        liftIO $ setSocketOption sock ReuseAddr 1 -- Useful for debugging
        liftIO $ setCloseOnExecIfNeeded <$> fdSocket sock
        D.bind (addrAddress addr)
        D.listen m
        forever $ do
          (conn, peer) <- D.accept
          liftIO $ putStrLn $ "Connection from " ++ show peer
          liftIO $ void $ forkFinally (talk conn) (\_ -> close conn)
      talk = flip serviceClient n

serviceClient conn n = do
  msg <- recv conn n
  unless (S.null msg) $ do
    sendAll conn msg
    serviceClient conn n
