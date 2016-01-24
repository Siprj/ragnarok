{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Network.SIP.Parser.RequestMethod
-- Description:  Request method parser.
-- Copyright:    Copyright (c) 2015-2016 Jan Sipr
-- License:      MIT
module Network.SIP.Parser.RequestMethod
    ( requestMethodParser
    )
  where

import Control.Applicative ((*>), pure)
import Data.Attoparsec.ByteString.Char8 (string)
import Data.Attoparsec.ByteString (Parser, choice)
import Data.ByteString.Char8 (pack)
import Data.Data (dataTypeConstrs, dataTypeOf, showConstr, fromConstr)
import Data.Function (($), (.))
import Data.Functor (fmap)

import Network.SIP.Type.RequestMethod (RequestMethod(ACK))

requestMethodParser :: Parser RequestMethod
requestMethodParser = choice . fmap toParser $ dataTypeConstrs (dataTypeOf ACK)
  where
    toParser x = (string . pack $ showConstr x) *> pure (fromConstr x)
