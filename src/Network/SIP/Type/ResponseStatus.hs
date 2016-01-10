{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Network.SIP.Type.ResponseStatus
-- Description:
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Big description.
module Network.SIP.Type.ResponseStatus
    ( ResponseCode(..)
    , Status(..)
    , UnknownResponseCode(..)
    , responseStatusMap
    )
  where

import Data.Data (Data)
import Data.Int (Int)
import Data.Eq (Eq, (==))
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Show (Show)

-- Performance test
import Control.DeepSeq (NFData)

data ResponseCode
    = OK_200
    | Ringing_180
    | SessionProgres_183
    | BadRequest_400
    | Unauthorized_401
    | Forbidden_403
    | Unknown UnknownResponseCode
  deriving (Show, Eq, Data, Generic, NFData)

-- Unknown messages must be moved somewhere else
data UnknownResponseCode
    = Unknown_1xx
    | Unknown_2xx
    | Unknown_3xx
    | Unknown_4xx
    | Unknown_5xx
    | Unknown_6xx
  deriving (Show, Eq, Data, Generic, NFData)

data Status = Status
    { statusCode :: ResponseCode
    , statusMsg :: Text
    }
  deriving (Show, Data, Generic, NFData)

instance Eq Status where
    a == b = statusCode a == statusCode b

-- | This list map's ResponseCode to its code value and descriptoin text.
-- (ResponseCode, (<code> :: Text, <descriptoin message> :: Text))
responseStatusMap :: [(ResponseCode, (Int, Text))]
responseStatusMap =
    [ (OK_200, (200, "OK"))
    , (Ringing_180, (180, "Ringing"))
    , (SessionProgres_183, (183, "Session Progress"))
    , (BadRequest_400, (400, "Bad Request"))
    , (Unauthorized_401, (401, "Unauthorized"))
    , (Forbidden_403, (403, "Forbidden"))
    ]
