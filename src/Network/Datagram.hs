{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

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


