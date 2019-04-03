{-# LANGUAGE Rank2Types #-}

module Bucket where

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
import Spec as S
import Utils

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

