{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Network.SIP.Type.RequestMethod
-- Description:  Enumeration of all possible request method.
-- Copyright:    Copyright (c) 2015-2016 Jan Sipr
-- License:      MIT
module Network.SIP.Type.RequestMethod
    ( RequestMethod(..)
    )
  where

import Data.Data (Data)
import Data.Eq (Eq)
import GHC.Generics (Generic)
import Prelude (Enum)
import Text.Show (Show)

-- Performance test
import Control.DeepSeq (NFData)

data RequestMethod
    = ACK
    | BYE
    | CANCEL
    | INFO
    | INVITE
    | MESSAGE
    | NOTIFY
    | OPTIONS
    | PRACK
    | PUBLISH
    | REFER
    | REGISTER
    | SUBSCRIBE
    | UPDATE
  deriving (Show, Eq, Enum, Data, Generic, NFData)
