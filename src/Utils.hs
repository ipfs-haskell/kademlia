module Utils where

import qualified Data.ByteString as BS

countDownToZero 0 = [0]
countDownToZero x = x : countDownToZero (x - 1)

byteStringToInteger :: BS.ByteString -> Integer
byteStringToInteger x = pack . process . unpack $ x
  where
    unpack = BS.unpack
    multF = map (\z -> 2 ^ (fst z * 8) * snd z)
    process = multF . zip (countDownToZero $ BS.length x - 1) . map fromIntegral
    pack = sum


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
				Just x  ->	let
								--TODO: newBucket = (deleteBy (\x y-> fst x == fst y) clientTriplet kBucket) ++ [clientTriplet]
								newBucket = moveToTail x bucket
								newNodeBuckets = (//) allBuckets [(bucketIndex, newBucket)]
							in
								node { nodeBuckets = newNodeBuckets }
				Nothing -> 	if length bucket < 4 then --TODO: use k parameter
								node { nodeBuckets = kBuckets ++ [clientTriplet]}		
							else
								--TODO: Handle this case
								if isNodeAlive (head bucket) then
									node { nodeBuckets = (//) allBuckets 
														[(bucketIndex, moveToTail 0 bucket)] 
										 }
								else
									node { nodeBuckets = (//) allBuckets 
														[(bucketIndex, tail bucket ++ [clientTriplet])] 
										 }
-- Moves from index to tail
moveToTail :: [a] -> Int -> [a]
moveToTail [] _ = []
moveToTail x index = let
						(p,q:r) = splitAt index x
					 in
					 	p ++ r ++ [q]

--TODO: complete it
isNodeAlive :: NodeTriplet -> Bool
isNodeAlive (id, Peer(ip, serviceName)) = True

-- 
findBucketIndex :: ID NodeID -> ID NodeID -> Int
findBucketIndex nodeID_ clientID = 
			let
				distance = safeXorByteString nodeID_ clientID
		    	bucketIndex = round . logBase 2 . fromInteger . byteStringToInteger $ distance
			in
				bucketIndex


unsafeXorByteString :: BS.ByteString -> BS.ByteString -> BS.ByteString
unsafeXorByteString a b = BS.pack $ BS.zipWith B.xor a b

safeXorByteString :: BS.ByteString -> BS.ByteString -> BS.ByteString
safeXorByteString x y
  | lx == ly = unsafeXorByteString x y
  | lx > ly = unsafeXorByteString y $ BS.take ly x
  | otherwise = unsafeXorByteString x $ BS.take lx y
  where
    lx = BS.length x
    ly = BS.length y