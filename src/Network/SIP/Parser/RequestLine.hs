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
    ( parseFirstLine
    )
  where

import Control.Applicative ((<*))
import Control.Exception (throwIO)
import Control.Monad (return, fail)
import Data.Attoparsec.ByteString.Char8 (string, takeTill, (<?>))
import Data.Attoparsec.ByteString (Parser, parseOnly)
import Data.ByteString (ByteString)
import Data.Either (Either(Right, Left))
import Data.Eq ((==))
import Data.Function (($), (.))
import Data.Monoid ((<>))
import Data.Text (unpack)
import System.IO (IO)

import Network.SIP.LowLevel.Type (InvalidMessage(BadRequestLine))
import Network.SIP.Parser.RequestMethod (requestMethodParser)
import Network.SIP.Parser.Uri (parseUri)
import Network.SIP.Type.RequestMethod (RequestMethod)
import Network.SIP.Type.Uri (Uri)

sipVersion :: ByteString
sipVersion = "SIP/2.0"

parseFirstLine :: ByteString -> IO (RequestMethod, Uri)
parseFirstLine x = do
    case parseOnly firstLineParser x of
        Right (m, u) -> return (m, u)
        Left s -> throwIO $ BadRequestLine s

firstLineParser :: Parser (RequestMethod, Uri)
firstLineParser = do
    method <- requestMethodParser <* string " " <?> "method parser"
    uri <- requestUri <?> "uri parser"
    return (method, uri)
  where
    requestUri :: Parser Uri
    requestUri = do
        us <- takeTill (== ' ') <* string (" " <> sipVersion)
        case parseUri us of
            (Left e) -> fail . unpack $ e <> "parseUri method failed"
            (Right u) -> return u

