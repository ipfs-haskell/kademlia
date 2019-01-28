module Spec where

import qualified Data.Bits as B
import qualified Data.ByteString as BS
import Data.Function (on)
import Data.List (sortBy)
import Network.Socket (SockAddr(..), HostAddress, PortNumber, SockAddr)

-- A phantom type to represent ID
type ID a = BS.ByteString

data NodeID

-- Change this, this is the UDP peer
data Peer = Peer
  { peerIP :: HostAddress
  , peerPort :: PortNumber
  }

peerToSockAddr :: Peer -> SockAddr
peerToSockAddr p = SockAddrInet thePeerPort thePeerIP
  where
    thePeerPort = peerPort p
    thePeerIP = peerIP p

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

sortBucketByDistance :: Bucket -> ID a -> Bucket
sortBucketByDistance bl id = unpack . sort . pack . bucketNodes $ bl
  where
    pack x = zip x $ map f x
    f = safeXorByteString id . nodeID
    sort = sortBy (compare `on` snd)
    unpack x = Bucket (bucketLogLower bl) $ map fst x

unsafeXorByteString :: BS.ByteString -> BS.ByteString -> BS.ByteString
unsafeXorByteString a b = snd $ BS.foldl f (b, BS.empty) a
  where
    f x y = (BS.tail . fst $ x, BS.snoc (snd x) $ xorResult x y)
    xorResult x y = B.xor y $ BS.head . fst $ x

safeXorByteString :: BS.ByteString -> BS.ByteString -> BS.ByteString
safeXorByteString x y
  | x >= y = unsafeXorByteString x y
  | otherwise = unsafeXorByteString y x
