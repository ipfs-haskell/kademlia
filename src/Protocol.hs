{-# LANGUAGE DeriveGeneric #-}

module Protocol where
import qualified Data.ByteString as BS
import Spec as S
import Data.Serialize as C
import GHC.Generics


type NodeTriplet = (ID NodeID, Peer)
type Message = BS.ByteString

data RPC = PING
  | PONG
  | STORE_REQUEST (ID Key) DataBlock
  | STORE_RESPONSE (ID Key) Message
  | FIND_NODE_REQUEST (ID NodeID)
  | FIND_NODE_RESPONSE [NodeTriplet]
  | FIND_VALUE_REQUEST (ID Key)
  | FIND_VALUE_RESPONSE Bool DataBlock [NodeTriplet] deriving Generic

answer :: RPC -> Node -> RPC
answer PING _ = PONG

