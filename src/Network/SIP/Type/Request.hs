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
import Network.URI (URI)
import Text.Show (Show)

import Network.SIP.Type.RequestMethod (RequestMethod)
import Network.SIP.Type.Header (Header)

data Request = Request
    { rqMethod :: RequestMethod
    , rqUri :: URI
    , rqHeaders :: [Header]
    , rqBody :: ByteString
    }
  deriving (Show)
