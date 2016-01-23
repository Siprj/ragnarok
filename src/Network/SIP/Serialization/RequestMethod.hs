{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Network.SIP.Serialize.RequestMethod
-- Description:  Serialize request method (INVITE, etc...).
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
module Network.SIP.Serialization.RequestMethod
    ( serializeRequestMethod
    )
  where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Data (toConstr, showConstr)
import Data.Function ((.))

import Network.SIP.Type.RequestMethod (RequestMethod)

serializeRequestMethod :: RequestMethod -> ByteString
serializeRequestMethod = pack . showConstr . toConstr
