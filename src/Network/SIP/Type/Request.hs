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
import Data.Maybe (Maybe)
import Text.Show (Show)

import Network.SIP.Type.Header (Header)
import Network.SIP.Type.RequestMethod (RequestMethod)
import Network.SIP.Type.Uri (Uri(Uri))

data Request = Request
    { rqMethod :: RequestMethod
    , rqUri :: Uri
    , rqHeaders :: [Header]
    , rqBody :: Maybe ByteString
    }
  deriving (Show)
