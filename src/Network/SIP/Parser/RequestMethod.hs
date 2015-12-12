{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Network.SIP.Parser.Header
-- Description:
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Big description.
module Network.SIP.Parser.RequestMethod
    ( requestMethodParser
    )
  where

import Control.Applicative ((*>), pure)
import Data.Attoparsec.ByteString (Parser, choice)
import Data.Attoparsec.ByteString.Char8 (string)
import Data.ByteString.Char8 (pack)
import Data.Data (dataTypeConstrs, dataTypeOf, showConstr, fromConstr)
import Data.Functor (fmap)
import Data.Function (($), (.))

import Network.SIP.Type.RequestMethod (RequestMethod(ACK))

requestMethodParser :: Parser RequestMethod
requestMethodParser = choice . fmap toParser $ dataTypeConstrs (dataTypeOf ACK)
  where
    toParser x = (string . pack $ showConstr x) *> pure (fromConstr x)
