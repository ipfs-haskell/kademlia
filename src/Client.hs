{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- {-# LANGUAGE KindSignatures #-}
module Client where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import qualified Data.ByteString as BS
import Network.Datagram as D
import Network.Socket as N
import Utils

-- | Sample send, modify this later.
-- runClient host port dgram = do
--   let hints = defaultHints {addrSocketType = N.Datagram}
--   addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
--   sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
--   D.runUdpT1024 (clientComputation addr dgram) sock

-- UdpT
clientComputation addr dgram = do
  sock <- lift ask
  liftIO $ N.connect sock addr
  D.send addr dgram
  (dgram, addr') <- D.receive
  liftIO $ printRecvDatagram (dgram, addr)
  liftIO $ N.close sock
