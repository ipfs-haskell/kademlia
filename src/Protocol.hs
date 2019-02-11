{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}

module Protocol where

import Control.Monad.Trans.State.Strict (StateT(..))
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Serialize as C
import Data.Vector ((!))
import GHC.Generics
import Spec as S
import Utils

type Message = BS.ByteString

data RPC
  = PING
  | PONG
  | STORE_REQUEST (ID Key)
                  DataBlock
  | STORE_RESPONSE (ID Key)
  | FIND_NODE_REQUEST (ID NodeID)
  | FIND_NODE_RESPONSE [NodeTriplet]
  | FIND_VALUE_REQUEST (ID Key)
  | FIND_VALUE_RESPONSE Bool
                        DataBlock
                        [NodeTriplet]
  deriving (Generic)

answer :: RPC -> NodeState RPC
answer PING = StateT $ \n -> pure (PONG, n)
answer (STORE_REQUEST i d) =
  StateT $ \n -> do
    let nHashTable = M.insert i d . nodeHashTable $ n
    let nN = n {nodeHashTable = nHashTable}
    return (STORE_RESPONSE i, nN)
answer (FIND_NODE_REQUEST id) =
  StateT $ \n -> do
    let diff = safeXorByteString id $ nodeID n
    let i = round . log . fromInteger . byteStringToInteger $ diff
    return (FIND_NODE_RESPONSE $ nodeBuckets n ! i, n)

nodeToTriplet :: Node -> NodeTriplet
nodeToTriplet n = (nodeID n, nodePeer n)
