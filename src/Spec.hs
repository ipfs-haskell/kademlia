module Spec where

import qualified Data.ByteString as BS
import qualified Data.Bits as B

-- A phantom type to represent ID
newtype ID a = ID BS.ByteString

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


sortBucketsByDistance :: [Bucket] -> ID a ->  [Bucket]
sortBucketsByDiatance bl (ID id) = unpack . sort . pack . bucketNodes $ bl
  where pack = zip bl $ map f bl
        f = xorByteString id . nodeID . removeIDConstructor
        sort = sortBy (compare `on` snd)
        unpack = map fst

xorByteString :: BS.ByteString -> BS.ByteString -> Integer
xorByteString a b = foldl f (b, BS.empty) a
  where f x y = (BS.tail . fst $ x, y `xor` BS.head . fst $ x)

