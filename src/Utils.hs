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
