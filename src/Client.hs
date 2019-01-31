{-# LANGUAGE OverloadedStrings #-}

-- Echo client program
module Client where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

msgTest :: String -> IO ()
msgTest x =
  withSocketsDo $ do
    addr <- resolve "127.0.0.1" "3000"
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
      sendAll sock $ C.pack x
      msg <- recv sock 1024
      putStr "Received: "
      C.putStrLn msg
