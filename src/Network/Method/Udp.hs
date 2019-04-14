{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Method.Udp where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import Data.Proxy
import GHC.TypeLits
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as Socket
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, unless, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import qualified Data.ByteString as S
import Network.Datagram as D
import Network.Socket as N
import Network.Socket.ByteString (recv, sendAll)
import Util
import Data.Serialize as Se
import Protocol.RPC
import Protocol.Spec

-- | A monad transformer that sends messages of at most n bytes as a
-- UDP packet.
newtype UdpT (n :: Nat) m a = UdpT
  { unUdpT :: ReaderT N.Socket m a
  } deriving (Functor, Applicative, Monad, MonadIO)

-- TODO: write a function to run an action in UdpT.
-- | Helper function to get the size of the Udp datagram.
messageSize :: (KnownNat n, Monad m) => UdpT n m Int
messageSize = fmap (fromEnum . natVal) getSizeProxy
  where
    getSizeProxy :: Monad m => UdpT n m (Proxy n)
    getSizeProxy = UdpT $ return Proxy

instance (KnownNat n, MonadIO m) => MonadDatagram (UdpT n m) where
  type Bound (UdpT n m) = n
  type Address (UdpT n m) = N.SockAddr
  send addr dgram = do
    sock <- UdpT ask
    liftIO $ Socket.sendAllTo sock (unwrap dgram) addr
  receive = do
    sock <- UdpT ask
    msize <- messageSize
    (msg, addr) <- liftIO $ Socket.recvFrom sock msize
    return (unsafeDatagram msg, addr)

-- | A server computation, initi and final
-- | ReaderT StateT UdpT ReaderT MaybeT
server addr computation
 = do
  sock <- lift . lift . lift $ ask
  liftIO . N.bind sock . addrAddress $ addr
  forever $ do
    d <- lift . lift $ D.receive
    computation d

-- | A client computation, init and final
-- | ReaderT StateT UdpT ReaderT MaybeT
client addr computation
  = do
   sock <- lift . lift . lift $ ask 
   liftIO $ N.connect sock addr
   computation
   liftIO $ N.close sock
    
