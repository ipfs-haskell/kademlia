module Protocol.Service where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Maybe
import qualified Network.Datagram as D
import Util
import Data.Serialize as Se
import Protocol.RPC
import Protocol.Spec

-- ReaderT StateT MethodT 
serverComputation (dgram, addr) = 
  case Se.decode . D.unwrap $ dgram of
    Left err -> liftIO $ print err
    Right rpc -> do
      ansRPC <- answer rpc
      case D.datagram . Se.encode $ ansRPC of
        Nothing -> liftIO $ print "Packet size is too large"
        Just ansDgram -> lift . lift . D.send addr $ ansDgram

-- ReaderT StateT MethodT
clientComputationRPC addr rpc = 
  case D.datagram . Se.encode $ rpc of
    Nothing -> liftIO $ print "Packet size is too large"
    (Just dgram) -> lift . lift $ D.send addr dgram













