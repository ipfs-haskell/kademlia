module Utils where

import qualified Data.ByteString as BS
import Spec as S
import qualified Network.Datagram as D

countDownToZero 0 = [0]
countDownToZero x = x : countDownToZero (x - 1)

byteStringToInteger :: BS.ByteString -> Integer
byteStringToInteger x = pack . process . unpack $ x
  where
    unpack = BS.unpack
    multF = map (\z -> 2 ^ (fst z * 8) * snd z)
    process = multF . zip (countDownToZero $ BS.length x - 1) . map fromIntegral
    pack = sum

idDiffLog :: ID a -> ID b -> Int
idDiffLog id1 = round . log . fromInteger . byteStringToInteger . safeXorByteString id1

printRecvDatagram (dgram, addr) = putStrLn $ show (D.unwrap dgram) ++ " << " ++ show addr
