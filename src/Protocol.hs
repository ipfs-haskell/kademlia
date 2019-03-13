{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}

module Protocol where

import Control.Monad.Trans.State.Strict as St
import Control.Monad.Trans.Reader
import qualified Data.ByteString as BS
import Data.List
import qualified Data.Map as M
import Data.Serialize as C
import Data.Vector ((!), (//), sum, toList)
import GHC.Generics
import Spec as S
import Utils
import Control.Monad.Trans.Class

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

answer PING = return PONG

answer (STORE_REQUEST i d) = do
  n <- lift St.get
  let nHashTable = M.insert i d . nodeHashTable $ n
  let nN = n {nodeHashTable = nHashTable}
  lift $ St.put nN
  return $ STORE_RESPONSE i

answer (FIND_NODE_REQUEST id) = do
  n <- lift St.get
  k <- fmap kGEnv ask
  kClosestNodes <- findKClosestNodes_ id
  return $ FIND_NODE_RESPONSE kClosestNodes

answer (FIND_VALUE_REQUEST key) = do
  n <- lift St.get
  k <- fmap kGEnv ask
  let value = M.lookup key (nodeHashTable n)
  return $ case value of
    Just v -> FIND_VALUE_RESPONSE True v []
    Nothing -> FIND_VALUE_RESPONSE False mempty $ findKClosestNodes n key k

nodeToTriplet :: Node -> NodeTriplet
nodeToTriplet n = (nodeID n, nodePeer n)

-- Updates Node's Buckets according to Protocol Rules.
-- Used when Node gets a request from some client
-- Or gets a reply from client
refreshBucket :: NodeTriplet -> Node -> Node
refreshBucket clientTriplet node =
  let 
    clientID = fst clientTriplet
    nodeID_ = nodeID node
    allBuckets = nodeBuckets node
    bucketIndex = idDiffLog nodeID_ clientID
    bucket = allBuckets ! bucketIndex --TODO: handle case when bucket doesnt exist
    --TODO: clientIndex = findIndex (\x -> (fst x) == clientID) bucket
    clientIndex = elemIndex clientTriplet bucket
  in case clientIndex of
        Just i
            --TODO: newBucket = (deleteBy (\x y-> fst x == fst y) clientTriplet kBucket) ++ [clientTriplet]
         ->
          let newBucket = moveIthToTail bucket i
              newNodeBuckets = allBuckets // [(bucketIndex, newBucket)]
           in node {nodeBuckets = newNodeBuckets}
        Nothing
          | length bucket < 4 --TODO: use k parameter
           ->
             node
              { nodeBuckets =
                  allBuckets // [(bucketIndex, bucket ++ [clientTriplet])]
              }
            --TODO: Handle this case
          | isNodeAlive (head bucket) ->
            node
              { nodeBuckets =
                  allBuckets // [(bucketIndex, moveIthToTail bucket 0)]
              }
          | otherwise ->
             node
              { nodeBuckets =
                  allBuckets //
                  [(bucketIndex, Data.List.tail bucket ++ [clientTriplet])]
              }

findKClosestNodes_ :: ID a -> NodeState [NodeTriplet]
findKClosestNodes_ id = do
  k <- fmap kGEnv ask
  n <- lift St.get
  let totalEntries = Data.Vector.sum . fmap Data.List.length . nodeBuckets $ n
  let allEntries = mconcat . toList . nodeBuckets $ n
  let bucketIndex = idDiffLog id . nodeID $ n
  let mainBucket = nodeBuckets n ! bucketIndex
  let allEntriesSorted = S.sortBucketByDistance allEntries id
  return $
    if totalEntries <= k then allEntries
    else if length mainBucket == k then mainBucket
    else take k allEntriesSorted
  
findKClosestNodes ::
     Node -- Node State
  -> ID a -- Client's ID
  -> Int -- k
  -> [NodeTriplet] -- k closest Entries
findKClosestNodes (Node nodeID_ buckets x y) id k
  | k <= 0 = []
  | totalEntries <= k = allEntries
  -- | When Client's corresponding bucket has k entries
  | length mainBucket == k = mainBucket
  | otherwise = take k allEntriesSorted
  where
    totalEntries = Data.Vector.sum $ fmap Data.List.length buckets
    allEntries = mconcat $ toList buckets
    bucketIndex = idDiffLog nodeID_ id
    mainBucket = buckets ! bucketIndex
    allEntriesSorted = S.sortBucketByDistance allEntries id

--TODO: complete it
isNodeAlive ::
     NodeTriplet -- Triplet of Node beint Pinged
  -> Bool -- Status of Ping
isNodeAlive (id, Peer ip serviceName) = True
