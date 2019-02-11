{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Client where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Network.Datagram as D
import Control.Monad.Trans.Reader
import Network.Socket as N
import Control.Monad.IO.Class (liftIO)

connect addr = do
  sock <- D.UdpT ask
  liftIO $ N.connect sock addr

send host port dgram = do
  let hints = defaultHints { addrSocketType = Datagram }
  addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  runComputation (c addr dgram) sock
  where
    c addr dgram = do
      Client.connect $ addrAddress addr
      flip D.send dgram $ addrAddress addr

runComputation :: UdpT 1024 IO a -> Socket -> IO a
runComputation c =
  runReaderT $ unUdpT c


