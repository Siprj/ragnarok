{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Network.SIP.Type.Request
-- Description:
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Big description.
module Network.SIP.Type.Request
    ( Request(..)
    )
  where

import Data.ByteString (ByteString)
import Data.Eq (Eq)
import Data.Data (Data)
import Data.Maybe (Maybe)
import GHC.Generics (Generic)
import Text.Show (Show)

import Network.SIP.Type.Header (Header)
import Network.SIP.Type.RequestMethod (RequestMethod)
import Network.SIP.Type.Uri (Uri(Uri))

-- Performance test
import Control.DeepSeq (NFData)

data Request = Request
    { rqMethod :: RequestMethod
    , rqUri :: Uri
    , rqHeaders :: [Header]
    , rqBody :: Maybe ByteString
    }
  deriving (Show, Generic, Data, NFData, Eq)
