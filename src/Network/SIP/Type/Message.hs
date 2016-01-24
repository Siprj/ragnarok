{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Network.SIP.Type.Message
-- Description:  Core type of SIP parser.
-- Copyright:    Copyright (c) 2015-2016 Jan Sipr
-- License:      MIT
--
-- Message type is core type of the whole parser. This type is the output from
-- parser and it is also the input to serialization.
module Network.SIP.Type.Message
    ( Message (..)
    , MessageType (..)
    )
  where

import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.Eq (Eq)
import Data.Maybe (Maybe)
import GHC.Generics (Generic)
import Text.Show (Show)

import Network.SIP.Type.Header (Header)
import Network.SIP.Type.RequestMethod (RequestMethod)
import Network.SIP.Type.ResponseStatus (Status)
import Network.SIP.Type.Uri (Uri)

-- Performance test
import Control.DeepSeq (NFData)

data MessageType =
    Request
        { rqMethod :: RequestMethod
        , rqUri :: Uri
        }
    | Response
        { rspStatus :: Status
        }
  deriving (Show, Generic, Data, NFData, Eq)

data Message = Message
    { msgType :: MessageType
    , msgHeaders :: [Header]
    , msgBody :: Maybe ByteString
    }
  deriving (Show, Generic, Data, NFData, Eq)
