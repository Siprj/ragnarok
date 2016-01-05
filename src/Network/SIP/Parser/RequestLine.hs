{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module:       Network.SIP.Parser.RequestLine
-- Description:
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Big description.
module Network.SIP.Parser.RequestLine
    ( firstLineParser
    )
  where

import Control.Applicative ((<*))
import Control.Monad (return, fail)
import Data.Attoparsec.ByteString.Char8 (char, string, takeTill, (<?>))
import Data.Attoparsec.ByteString (Parser)
import Data.Either (Either(Right, Left))
import Data.Eq ((==))
import Data.Function (($), (.))
import Data.Monoid ((<>))
import Data.Text (unpack)

import Network.SIP.Parser.RequestMethod (requestMethodParser)
import Network.SIP.Parser.SipVersion (sipVersionParser)
import Network.SIP.Parser.Uri (parseUri)
import Network.SIP.Type.RequestMethod (RequestMethod)
import Network.SIP.Type.Uri (Uri)

firstLineParser :: Parser (RequestMethod, Uri)
firstLineParser = do
    method <- requestMethodParser <* char ' ' <?> "method parser"
    uri <- requestUri
    return (method, uri)
  where
    requestUri :: Parser Uri
    requestUri = do
        us <- takeTill (== ' ') <* char ' ' <* sipVersionParser
        case parseUri us of
            (Left e) -> fail . unpack $ e <> "parseUri method failed"
            (Right u) -> return u
