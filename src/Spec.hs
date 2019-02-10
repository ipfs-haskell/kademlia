{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}


module Spec where

import qualified Data.Bits as B
import qualified Data.ByteString as BS
import Data.Function (on)
import Data.List (sortBy)
import Network.Socket (SockAddr(..), HostAddress, ServiceName, SockAddr)
-- Change to Lazy Map later
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad

-- A local hash table
type HashTable = M.Map (ID Key) DataBlock

type DataBlock = BS.ByteString

-- A phantom type to represent ID
type ID a = BS.ByteString

data NodeID
data Key

-- Change this, this is the UDP peer
data Peer = Peer
  { peerIP :: HostAddress
  , peerPort :: ServiceName
  }


-- A node in the Kademlia Network
data Node = Node
  { nodeID :: ID NodeID
  , nodeBuckets :: V.Vector Bucket
  , nodeHashTable :: HashTable
  , nodePeer :: Peer
  }

type NodeState a = forall m. Monad m => StateT Node m a

type NodeTriplet = (ID NodeID, Peer)

-- A bucket representation
type Bucket = [NodeTriplet]

sortBucketByDistance :: Bucket -> ID a -> Bucket
sortBucketByDistance bl id = unpack . sort . pack $ bl
  where
    pack x = zip x $ map f x
    f = safeXorByteString id . fst
    sort = sortBy (compare `on` snd)
    unpack = map fst

unsafeXorByteString :: BS.ByteString -> BS.ByteString -> BS.ByteString
unsafeXorByteString a b = BS.pack $ BS.zipWith B.xor a b

safeXorByteString :: BS.ByteString -> BS.ByteString -> BS.ByteString
safeXorByteString x y
  | lx == ly = unsafeXorByteString x y
  | lx > ly = unsafeXorByteString y $ BS.take ly x
  | otherwise = unsafeXorByteString x $ BS.take lx y
  where 
    lx = BS.length x
    ly = BS.length y
