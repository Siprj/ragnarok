{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Network.SIP.Type.TransportProtocol
-- Description:
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Big description.
module Network.SIP.Type.TransportProtocol
    ( TransportProtocol(..)
    )
  where

import Data.Eq (Eq)
import Data.Data (Data)
import GHC.Generics (Generic)
import Text.Show (Show)

-- Performance test
import Control.DeepSeq (NFData)

data TransportProtocol = TCP | UDP | TLS | SCTP
  deriving (Show, Eq, Data, Generic, NFData)
