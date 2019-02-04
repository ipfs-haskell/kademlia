{-# LANGUAGE DeriveGeneric #-}

module Protocol where
import qualified Data.ByteString as BS
import Spec as S
import Data.Serialize as C
import GHC.Generics
import qualified Data.Map as M


type NodeTriplet = (ID NodeID, Peer)
type Message = BS.ByteString

data RPC = PING
  | PONG
  | STORE_REQUEST (ID Key) DataBlock
  | STORE_RESPONSE (ID Key)
  | FIND_NODE_REQUEST (ID NodeID)
  | FIND_NODE_RESPONSE [NodeTriplet]
  | FIND_VALUE_REQUEST (ID Key)
  | FIND_VALUE_RESPONSE Bool DataBlock [NodeTriplet] deriving Generic

answer :: RPC -> Node -> (RPC, Node)
answer PING n = (PONG, n)
answer (STORE_REQUEST i d) n = (STORE_RESPONSE i, nN)
  where
    cHashTable = nodeHashTable n
    nHashTable = M.insert i d cHashTable
    nN = n { nodeHashTable = nHashTable }
