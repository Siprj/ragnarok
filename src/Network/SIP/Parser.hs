{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module:       Network.SIP.Parser
-- Description:
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Big description.
module Network.SIP.Parser
    ( typeHeader
    )
  where

import Control.Exception (throwIO)
import Control.Monad (return)
import Data.Maybe (maybe)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Text.Encoding (decodeUtf8)
import System.IO (IO)
import Data.Tuple (swap)

import Network.SIP.Type.Header
    ( Header(Header)
    , headerNameMap)
import Network.SIP.LowLevel.Type
    ( InvalidMessage(WrongHeader)
    )

import Data.List (lookup)
import qualified Network.SIP.LowLevel.Type as LL (Header)

typeHeader :: LL.Header -> IO Header
typeHeader (h, v) =
    maybe (throwIO WrongHeader) return $
        fmap (\x -> Header x (decodeUtf8 v)) . lookup h . fmap swap $ headerNameMap
