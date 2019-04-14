{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}

module Protocol.RPC where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict as St
import qualified Data.ByteString as BS
import Data.List
import qualified Data.Map as M
import Data.Serialize as C
import Data.Vector ((!), (//), sum, toList)
import GHC.Generics
import Protocol.Spec as S
import Util
import Data.Serialize as Se

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
  deriving (Generic, Show)

instance Serialize RPC

answer :: RPC -> ComputationEnv RPC
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
  return $
    case value of
      Just v -> FIND_VALUE_RESPONSE True v []
      Nothing -> FIND_VALUE_RESPONSE False BS.empty $ findKClosestNodes n key k

nodeToTriplet :: Node -> NodeTriplet
nodeToTriplet n = (nodeID n, nodePeer n)


findKClosestNodes_ :: ID a -> ComputationEnv [NodeTriplet]
findKClosestNodes_ id = do
  k <- fmap kGEnv ask
  n <- lift St.get
  let allEntries = mconcat . toList . nodeBuckets $ n
  let bucketIndex = idDiffLog id . nodeID $ n
  let mainBucket = nodeBuckets n ! bucketIndex
  let allEntriesSorted = S.sortBucketByDistance allEntries id
  return $ caseReturn k mainBucket allEntriesSorted
  where
    caseReturn k mainBucket allEntriesSorted
      | length allEntriesSorted <= k = allEntriesSorted
      | length mainBucket == k = mainBucket
      | otherwise = take k allEntriesSorted

-- | Parth's implementation, I've modified it as above
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


