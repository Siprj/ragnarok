{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:       Network.SIP.Serialization.Uri
-- Description:  Serialize URI into ByteString.
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
module Network.SIP.Serialization.Uri
    ( serializeUri
    )
  where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Function (($))
import Data.Maybe (Maybe(Nothing, Just))
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)
import Data.Word (Word16)
import Text.Show (show)

import Network.SIP.Type.Uri
    ( Scheme(SIP, SIPS)
    , Uri(Uri)
    , uriScheme
    , uriUser
    , uriHost
    , uriPort
    )

serializeScheme :: Scheme -> ByteString
serializeScheme SIP = "sip:"
serializeScheme SIPS = "sips:"

serializeUser :: Maybe Text -> ByteString
serializeUser (Just t) = encodeUtf8 t <> "@"
serializeUser (Nothing) = ""

serializePort :: Maybe Word16 -> ByteString
serializePort (Nothing) = ""
serializePort (Just t) = ":" <> (pack $ show t)
  -- This pack and show can by probably optimized by some another function.

serializeUri :: Uri -> ByteString
serializeUri Uri{..} =
    serializeScheme uriScheme
    <> serializeUser uriUser
    <> encodeUtf8 uriHost
    <> serializePort uriPort
