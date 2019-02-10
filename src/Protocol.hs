{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}

module Protocol where
import qualified Data.ByteString as BS
import Spec as S
import Data.Serialize as C
import GHC.Generics
import qualified Data.Map as M
import Control.Monad.Trans.State.Strict (StateT(..))
import Data.Vector ((!))

type Message = BS.ByteString

data RPC = PING
  | PONG
  | STORE_REQUEST (ID Key) DataBlock
  | STORE_RESPONSE (ID Key)
  | FIND_NODE_REQUEST (ID NodeID)
  | FIND_NODE_RESPONSE [NodeTriplet]
  | FIND_VALUE_REQUEST (ID Key)
  | FIND_VALUE_RESPONSE Bool DataBlock [NodeTriplet] deriving Generic

countDownToZero 0 = [0]
countDownToZero x = x:countDownToZero (x - 1)
  
byteStringToInteger :: BS.ByteString -> Integer
byteStringToInteger x = pack . process . unpack $ x
  where
    unpack = BS.unpack
    multF = map (\z -> 2 ^ (fst z * 8) * snd z)
    process = multF . zip (countDownToZero $ BS.length x - 1) . map fromIntegral
    pack = sum
    
answer :: RPC -> NodeState RPC
answer PING = StateT $ \n -> pure (PONG, n)
answer (STORE_REQUEST i d) = StateT $ \n -> do
  let nHashTable = M.insert i d . nodeHashTable $ n
  let nN = n { nodeHashTable = nHashTable }
  return (STORE_RESPONSE i, nN)
answer (FIND_NODE_REQUEST id) = StateT $ \n -> do
  let diff = safeXorByteString id $ nodeID n
  let i = round . log . fromInteger . byteStringToInteger $ diff
  return (FIND_NODE_RESPONSE $ nodeBuckets n ! i, n)

nodeToTriplet :: Node -> NodeTriplet
nodeToTriplet n = (nodeID n, nodePeer n)


