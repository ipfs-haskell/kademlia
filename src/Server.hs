module Server where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, unless, void)
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

type NumConn = Int

type NumBytes = Int

runServer :: ServiceName -> NumConn -> NumBytes -> IO ()
runServer s m n port = do
    let hints =
          defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Datagram}
    addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    D.runUdpT1024 (c addr dgram) sock
    where
      c = do
      liftIO $ setSocketOption sock ReuseAddr 1 -- Useful for debugging
      liftIO $ fmap setCloseOnExec $ fdSocket sock
      bind (addrAddress addr)
      listen m
      forever $ do
        (conn, peer) <- accept
        putStrLn $ "Connection from " ++ show peer
        void $ forkFinally (talk conn) (\_ -> close conn)
    talk = flip serviceClient n

serviceClient conn n = do
  msg <- recv conn n
  unless (S.null msg) $ do
    sendAll conn msg
    serviceClient conn n
