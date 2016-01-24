{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:       Network.SIP.Serialize.FirstLine
-- Description:  Serialize first line of SIP message.
-- Copyright:    Copyright (c) 2015-2016 Jan Sipr
-- License:      MIT
module Network.SIP.Serialization.FirstLine
    ( serializeFirstLine
    )
  where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unwords)

import Network.SIP.Serialization.RequestMethod (serializeRequestMethod)
import Network.SIP.Serialization.Status (serializeStatus)
import Network.SIP.Serialization.Uri (serializeUri)
import Network.SIP.Type.Message
    ( MessageType(Request, Response)
    , rqMethod
    , rqUri
    , rspStatus
    )

sipVersion :: ByteString
sipVersion = "SIP/2.0"

serializeFirstLine :: MessageType -> ByteString
serializeFirstLine Request{..} = unwords
    [ serializeRequestMethod rqMethod
    , serializeUri rqUri
    , sipVersion
    ]
serializeFirstLine Response{..} = unwords
    [ sipVersion
    , serializeStatus rspStatus
    ]
