{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Network.SIP.Type.TransportProtocol
-- Description:  Transports which are supported by SIP transmission.
-- Copyright:    Copyright (c) 2015-2016 Jan Sipr
-- License:      MIT
module Network.SIP.Type.TransportProtocol
    ( TransportProtocol(..)
    )
  where

import Data.Data (Data)
import Data.Eq (Eq)
import GHC.Generics (Generic)
import Text.Show (Show)

-- Performance test
import Control.DeepSeq (NFData)

data TransportProtocol = TCP | UDP | TLS | SCTP
  deriving (Show, Eq, Data, Generic, NFData)
