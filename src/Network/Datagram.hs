-- Written by Piyush Kurur (github.com/piyush-kurur) and Adithya Kumar (github.com/adithyaov)

{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
  GeneralizedNewtypeDeriving, KindSignatures, TypeFamilies #-}

module Network.Datagram where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader
import qualified Data.ByteString as BS
import Data.Proxy
import GHC.TypeLits
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as Socket

-- | A datagram of size at most bound
newtype Datagram (bound :: Nat) = Datagram
  { unwrap :: BS.ByteString
  }

-- | Get the size
sizeBound :: KnownNat bound => Proxy (Datagram bound) -> Int
sizeBound = fromEnum . natVal . boundProxy
  where
    boundProxy :: Proxy (Datagram bound) -> Proxy bound
    boundProxy _ = Proxy

-- | Make a package out of the byte string.
datagram :: KnownNat bound => BS.ByteString -> Maybe (Datagram bound)
datagram bs
  | BS.length bs <= sizeBound (pure dgram) = Just dgram
  | otherwise = Nothing
  where
    dgram = Datagram bs

-- | Function that creates a datagram without checking for bounds. One
-- should avoid using this function.
unsafeDatagram :: BS.ByteString -> Datagram bound
unsafeDatagram = Datagram

-- | A monad which allows sending and receiving datagrams.
class (KnownNat (Bound m), MonadIO m) =>
      MonadDatagram m
  -- | The bound on the datagram size.
  where
  type Bound m :: Nat
  -- | Associated type that captures address
  type Address m :: *
  -- | Send a datagram to a given address.
  send :: Address m -> Datagram (Bound m) -> m ()
  -- | Receive a datagram. The address of the peer is also given.
  receive :: m (Datagram (Bound m), Address m)

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

-- | Running a UdpT computation with an environment
runUdpT1024 :: UdpT 1024 IO a -> N.Socket -> IO a
runUdpT1024 c =
  runReaderT $ unUdpT c

-- | Connecting to a remote socket
close :: (MonadIO m) => UdpT n m ()
connect addr = do
  sock <- UdpT ask
  liftIO $ N.connect sock addr

-- | Closing a socket
close :: (MonadIO m) => UdpT n m ()
close = do
  sock <- UdpT ask
  liftIO $ N.close sock

