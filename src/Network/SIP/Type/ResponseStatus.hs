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
    ( Status(..)
    , ResponseCode(..)
    , responseStatusMap
    )
  where

import Data.Data (Data)
import Data.Eq (Eq)
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
    { statusCode :: Text
    , statusMsg :: Text
    }
  deriving (Show, Eq, Data, Generic, NFData)

responseStatusMap :: [(ResponseCode, Status)]
responseStatusMap =
    [ (OK_200, Status "200" "OK")
    , (Ringing_180, Status "180" "Ringing")
    , (SessionProgres_183, Status "183" "Session Progress")
    , (BadRequest_400, Status "400" "Bad Request")
    , (Unauthorized_401, Status "401" "Unauthorized")
    , (Forbidden_403, Status "403" "Forbidden")
    ]
