{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Network.SIP.Serialization.Header
-- Description:  SIP header serialization.
-- Copyright:    Copyright (c) 2015-2016 Jan Sipr
-- License:      MIT
--
-- Serialize SIP header into transportable form.
module Network.SIP.Serialization.Header
    ( serializeHeader
    )
  where

import Data.ByteString (ByteString)
import Data.CaseInsensitive (original)
import Data.Function (($), (.))
import Data.List (lookup)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)

import Network.SIP.Type.Header
    ( Header
    , HeaderName (Custom)
    , headerNameMap
    )

serializeHeader :: Header -> ByteString
serializeHeader (Custom t, v) = encodeUtf8 $ "X-" <> t <> ": " <> v
serializeHeader (h, v) =
    -- TODO: Function fromJust in this section is not good, it needs to be
    -- rewritten and some sort of exception handling must be "invented".
    (original . fromJust $ lookup h headerNameMap) <> ": " <>
    encodeUtf8 v
