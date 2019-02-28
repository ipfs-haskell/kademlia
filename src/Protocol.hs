{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}

module Protocol where

import Control.Monad.Trans.State.Strict (StateT(..))
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Serialize as C
import Data.List
import Data.Vector ((!),(//),sum, toList)
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
    let kClosestNodes = findKClosestNodes n id 4 --TODO: use Global Parameter k
    return (FIND_NODE_RESPONSE $ kClosestNodes, n)

answer (FIND_VALUE_REQUEST key) = 
  StateT $ \n -> do
    let value = M.lookup key (nodeHashTable n)
    case value of 
      Just v  ->  return (FIND_VALUE_RESPONSE True v [], n)
      Nothing ->  
                  let
                    kClosestNodes = findKClosestNodes n key 4 --TODO: use Global Parameter k
                  in
                    return (FIND_VALUE_RESPONSE False mempty kClosestNodes, n)

nodeToTriplet :: Node -> NodeTriplet
nodeToTriplet n = (nodeID n, nodePeer n)

-- Updates Node's Buckets according to Protocol Rules.
-- Used when Node gets a request from some client
-- Or gets a reply from client
refreshBucket :: Node -> NodeTriplet -> Node
refreshBucket node clientTriplet = 
  let
    clientID = fst $ clientTriplet
    nodeID_ = nodeID node
    allBuckets = nodeBuckets node
    bucketIndex = findBucketIndex nodeID_ clientID
    bucket = allBuckets ! bucketIndex --TODO: handle case when bucket doesnt exist
    --TODO: clientIndex = findIndex (\x -> (fst x) == clientID) bucket
    clientIndex = elemIndex clientTriplet bucket
  in
    case clientIndex of 
        Just i  ->  
            let
            --TODO: newBucket = (deleteBy (\x y-> fst x == fst y) clientTriplet kBucket) ++ [clientTriplet]
              newBucket = moveIthToTail bucket i
              newNodeBuckets = allBuckets // [(bucketIndex, newBucket)]
            in
              node { nodeBuckets = newNodeBuckets }
        Nothing ->  
            if length bucket < 4 --TODO: use k parameter
              then node { nodeBuckets = allBuckets // [(bucketIndex, bucket ++ [clientTriplet])] }   
            --TODO: Handle this case
            else 
              if isNodeAlive (head bucket) then 
                node { nodeBuckets =  allBuckets // [(bucketIndex, moveIthToTail bucket 0)] }
              else
                node { nodeBuckets = allBuckets // [ ( bucketIndex, (Data.List.tail bucket)++[clientTriplet] ) ] }



findKClosestNodes ::    Node           -- Node State
                    ->  ID a           -- Client's ID
                    ->  Int            -- k
                    ->  [NodeTriplet]  -- k closest Entries

findKClosestNodes (Node nodeID_ buckets x y) id k
  | k <= 0                  = []
  | totalEntries <= k       = allEntries 
  -- | When Client's corresponding bucket has k entries
  | length mainBucket == k  = mainBucket
  | otherwise               = take k allEntriesSorted

  where
    totalEntries      = Data.Vector.sum $ fmap Data.List.length buckets
    allEntries        = mconcat $ toList buckets
    bucketIndex       = findBucketIndex nodeID_ id
    mainBucket        = buckets ! bucketIndex
    allEntriesSorted  = S.sortBucketByDistance allEntries id


--TODO: complete it
isNodeAlive ::  NodeTriplet -- Triplet of Node beint Pinged
            ->  Bool        -- Status of Ping
isNodeAlive (id, Peer ip serviceName) = True


-- It finds index of bucket where client belongs for given Node.
findBucketIndex ::   ID NodeID  -- Node ID
                  -> ID NodeID  -- Client ID
                  -> Int        -- Bucket Index

findBucketIndex nodeID_ clientID = 
      let
        distance = S.safeXorByteString nodeID_ clientID
        bucketIndex = round . logBase 2 . fromInteger . byteStringToInteger $ distance
      in
        bucketIndex
