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
runServer s m n =
  withSocketDo $ do
    addr <- resolve s
    E.bracket (open addr) close loop
  where
    resolve port = do
      let hints =
            defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Datagram}
      addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1 -- Useful for debugging
      fd <- fdSocket sock
      setCloseOnExecIfNeeded fd
      bind sock (addrAddress addr)
      listen sock m
      return sock
    loop sock =
      forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Connection from " ++ show peer
        void $ forkFinally (talk conn) (\_ -> close conn)
    talk = flip serviceClient n

serviceClient conn n = do
  msg <- recv conn n
  unless (S.null msg) $ do
    sendAll conn msg
    serviceClient conn n
