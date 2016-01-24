{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:       Network.SIP.Serialize.Status
-- Description:  Serialize SIP response status.
-- Copyright:    Copyright (c) 2015-2016 Jan Sipr
-- License:      MIT
module Network.SIP.Serialization.Status
    ( serializeStatus
    )
  where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unwords, pack)
import Data.Function ((.), ($))
import Data.List (lookup)
import Data.Maybe (fromJust)
import Data.Text.Encoding (encodeUtf8)
import Data.Tuple (uncurry)
import Text.Show (show)

import Network.SIP.Type.ResponseStatus
    ( Status(Status)
    , responseStatusMap
    , statusCode
    , statusMsg
    )

serializeStatus :: Status -> ByteString
serializeStatus Status{..} =
    uncurry magic . fromJust $ lookup statusCode responseStatusMap
  where
    magic a b = unwords
        [ pack $ show a
        , encodeUtf8 b
        ]
