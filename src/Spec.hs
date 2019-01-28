module Spec where

import qualified Data.Bits as B
import qualified Data.ByteString as BS
import Data.List (sortBy)
import Data.Function (on)

-- A phantom type to represent ID
newtype ID a =
  ID BS.ByteString

data NodeID

-- Change this, this is the UDP peer
data Peer = Peer
  { peerIP :: String
  , peerPort :: String
  }

-- A node in the Kademlia Network
data Node = Node
  { nodeID :: ID NodeID
  , nodeBuckets :: [Bucket]
  , nodePeer :: Peer
  }

-- A bucket representation
data Bucket = Bucket
  { bucketLogLower :: Integer
  , bucketNodes :: [Node]
  }

removeIDConstructor (ID x) = x

sortBucketsByDistance :: Bucket -> ID a -> Bucket
sortBucketByDiatance bl (ID id) = unpack . sort . pack . bucketNodes $ bl
  where
    pack x = zip x $ map f x
    f = unsafeXorByteString id . removeIDConstructor . nodeID
    sort = sortBy (compare `on` snd)
    unpack x = Bucket (bucketLogLower bl) $ map fst x

unsafeXorByteString :: BS.ByteString -> BS.ByteString -> BS.ByteString
unsafeXorByteString a b = snd $ BS.foldl f (b, BS.empty) a
  where
    f x y = (BS.tail . fst $ x, BS.snoc (snd x) $ xorResult x y)
    xorResult x y = B.xor y $ BS.head . fst $ x
