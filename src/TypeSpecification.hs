-- Parametrize over String
module TypeSpecification where

import Network.Socket (PortNumber, SockAddr(..))

-- Signature of a UDP peer
class UDPPeer a where
  host :: a -> String
  port :: a -> PortNumber
  toUDPPeer :: SockAddr -> IO (Maybe a)

-- Signature of an Xor distance
class XorDistance a where
  xor :: a -> a -> Ordering

-- Signature of any Node
class (XorDistance b, UDPPeer c) =>
      Node a
  where
  udpPeer :: a -> c
  nodeId :: a -> b

-- Sort set of nodes (bucket) by their Xor distance
sortByXor :: (Node a, XorDistance b) => [a] -> b -> [a]
sortByXor bucket id =
  let pack bk = zip bk $ map f bk
      f = xor id . nodeId
      sort = sortBy (compare `on` snd)
      unzip = map fst
   in unpack . sort . pack $ bucket

-- A Protocol signal
data Signal i a b = Signal
  { source :: a
  , command :: Command i a b
  }

-- Protocol Commands in IPFS
data Command i a b
        -- The basic PING and PONG
  = PING
  | PONG
        -- Command to store a datablock `b` on a Node with id `i`
  | STORE i
          b
        -- Command to find a node with id `i`
  | FIND_NODE i
        -- Response to FIND_NODE
  | RETURN_NODES i
                 [a]
        -- Command to find a datablock with key `i`
  | FIND_VALUE i
        -- Response to FIND_VALUE
  | RETURN_VALUE i
                 b
