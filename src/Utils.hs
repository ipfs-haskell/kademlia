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


-- Moves from index to tail
moveIthToTail :: 	[a] -- Original List
				-> 	Int -- Index of the Item to be shifted at the tail
				-> 	[a] -- Final List

moveIthToTail [] _ = []
moveIthToTail x index = 
	| index < 0 || index >= length x	=	x
	| otherwise							=
											let
												(p,q:r) = splitAt index x
											in
												p ++ r ++ [q]
