module Protocol where
import qualified Data.ByteString as BS
import Spec as S

type NodeTriplet = (ID NodeID, Peer)
type DataBlock = BS.ByteString
type Message = BS.ByteString

data RPC = PING
  | PONG
  | STORE_REQUEST (ID Key) DataBlock
  | STORE_RESPONSE (ID Key) Message
  | FIND_NODE_REQUEST (ID NodeID)
  | FIND_NODE_RESPONSE [NodeTriplet]
  | FIND_VALUE_REQUEST (ID Key)
  | FIND_VALUE_RESPONSE Bool DataBlock [NodeTriplet]


