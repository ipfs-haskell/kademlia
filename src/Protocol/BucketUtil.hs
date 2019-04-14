{-# LANGUAGE Rank2Types #-}

module Protocol.BucketUtil where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Strict as St
import Control.Monad.Trans.Reader
import qualified Data.ByteString as BS
import Data.List
import qualified Data.Map as M
import Data.Serialize as C
import Data.Vector ((!), (//))
import GHC.Generics
import Protocol.Spec as S
import Util

moveItemToEndIfExists i [] = []
moveItemToEndIfExists i (x:xs)
  | i == x = xs ++ [i]
  | otherwise = x:moveItemToEndIfExists i xs

addIfNotInTail i [] = [i]
addIfNotInTail i xs
  | last xs == i = xs
  | otherwise = xs ++ [i]

isNodeAlive t = return True

removeTripletAfterCheck True b = init b 
removeTripletAfterCheck False b = tail b 

bucketLimit l b
  | length b < l = return b
  | otherwise = fmap (`removeTripletAfterCheck` b) $ isNodeAlive $ head b

-- | Buckets always exist, they might be empty
refreshBucket :: NodeTriplet -> ComputationEnv ()
refreshBucket t = do
  n <- lift St.get
  k <- fmap kGEnv ask
  let cID = fst t
  let nID = nodeID n
  let allBuckets = nodeBuckets n
  let bucketIndex = idDiffLog nID cID
  let bucket = allBuckets ! bucketIndex
  modifiedBucket <- liftIO $ bucketLimit k . addIfNotInTail t . moveItemToEndIfExists t $ bucket
  lift . St.put $ n { nodeBuckets = allBuckets // [(bucketIndex, modifiedBucket)] }

-- | Parth's implementation, I've simplified it as above, Credits to parth though :-)

-- Updates Node's Buckets according to Protocol Rules.
-- Used when Node gets a request from some client
-- Or gets a reply from client
refreshBucket' :: NodeTriplet -> Node -> Node
refreshBucket' clientTriplet node =
  let clientID = fst clientTriplet
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
          | isNodeAlive' (head bucket) ->
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



--TODO: complete it
isNodeAlive' ::
     NodeTriplet -- Triplet of Node beint Pinged
  -> Bool -- Status of Ping
isNodeAlive' (id, Peer ip serviceName) = True
