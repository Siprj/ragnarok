{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:       Network.SIP.Serialization
-- Description:  SIP message serialization.
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Serialize SIP message int to transportable form.
module Network.SIP.Serialization
    ( serializeSipMessage
    )
  where

import Data.ByteString (ByteString)
import Data.Foldable (foldl1)
import Data.Function (($))
import Data.Functor (fmap)
import Data.Maybe (maybe)
import Data.Monoid ((<>))

import Network.SIP.Type.Message
    ( Message(Message)
    , msgType
    , msgHeaders
    , msgBody
    )
import Network.SIP.Serialization.FirstLine (serializeFirstLine)
import Network.SIP.Serialization.Header (serializeHeader)

unlinesCRLF :: [ByteString] -> ByteString
unlinesCRLF = foldl1 cn
  where
    cn a b = a <> "\r\n" <> b

serializeSipMessage :: Message -> ByteString
serializeSipMessage Message{..} =
    maybe messageHeader (messageHeader <>) msgBody
  where
    messageHeader = unlinesCRLF
        [ serializeFirstLine msgType
        , unlinesCRLF $ fmap serializeHeader msgHeaders
        , "\r\n" -- This will make and of SIP Header (\r\n\r\n)
        ]
