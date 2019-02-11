{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Client where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Network.Datagram

test :: UdpT 1024 m a
