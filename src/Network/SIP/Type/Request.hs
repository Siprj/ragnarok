{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Network.SIP.Type.Header (HeaderField)

data Request = Request
    { rqMethod :: RequestMethod
    , rqUri :: URI
    , rqHeaders :: [HeaderField]
    , rqBody :: ByteString
    }
  deriving (Show)
