{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Network.SIP.Type.RequestMethod
-- Description:
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Big description.
module Network.SIP.Type.RequestMethod
    ( RequestMethod(..)
    )
  where

import Data.Eq (Eq)
import Data.Data (Data)
import Prelude (Enum)
import GHC.Generics (Generic)
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
