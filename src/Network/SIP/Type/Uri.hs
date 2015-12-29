{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Network.SIP.Type.URI
-- Description:  Sip uri type
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Uri from from Network.URI is not able correctly parser sip:name@host format.
-- This data type and belonging parser is here to fill the gap.
module Network.SIP.Type.Uri
    ( Uri(..)
    , Scheme(..)
    )
  where

import Data.Data (Data)
import Data.Eq (Eq)
import Data.Maybe (Maybe)
import Data.Ord (Ord)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Word (Word16)
import GHC.Generics (Generic)
import Text.Show (Show)

-- Performance test
import Control.DeepSeq (NFData)

data Scheme = SIP | SIPS
  deriving (Show, Eq, Data, Ord, Generic, Typeable, NFData)

data Uri = Uri
    { uriScheme :: Scheme
    , uriUser :: Maybe Text
    , uriHost :: Text
    , uriPort :: Maybe Word16
    }
  deriving (Show, Eq, Data, Generic, Typeable, NFData)
