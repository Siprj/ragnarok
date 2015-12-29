{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module:       Network.SIP.Parser.Request
-- Description:
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Big description.
module Network.SIP.Parser.Request
    ( parseRequest
    )
  where

import Control.Applicative ((<$>), (<*), (<*>), (*>), many)
import Control.Monad (return, fail)
import Data.Attoparsec.ByteString.Char8 (string, takeTill, (<?>), take)
import Data.Attoparsec.ByteString (Parser, takeByteString)
import Data.ByteString (ByteString)
import Data.Either (Either(Right, Left))
import Data.Eq ((==))
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Int (Int)
import Data.List (find)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Monoid ((<>))
import Data.Text (unpack)
import Data.Text.Read (decimal)

import Network.SIP.Type.Header (Header(fieldName, fieldValue), HeaderName(..), )
import Network.SIP.Type.RequestMethod (RequestMethod)
import Network.SIP.Type.Request (Request(Request))
import Network.SIP.Type.Uri (Uri(Uri))

import Network.SIP.Parser.Header (headerParser)
import Network.SIP.Parser.RequestMethod (requestMethodParser)
import Network.SIP.Parser.Uri (parseUri)

sipVersion :: ByteString
sipVersion = "SIP/2.0"

parseRequest :: Parser Request
parseRequest = do
    method <- requestMethodParser <* string " " <?> "method parser"
    uri <- requestUri <?> "uri parser"
    headers <- many headerParser <?> "headers parser"
    cl <- lookupContentLength headers
    body <- bodyParser cl
    return $ Request method uri headers body
  where
    requestUri :: Parser Uri
    requestUri = do
        us <- takeTill (== ' ') <* string (" " <> sipVersion <> "\r\n")
        case parseUri us of
            (Left e) -> fail . unpack $ e <> "parseUri method failed"
            (Right u) -> return u

    bodyParser :: Maybe Int -> Parser (Maybe ByteString)
    bodyParser cl = string "\r\n"
        *> maybe (return Nothing) (fmap (Just) . take) cl

    lookupContentLength :: [Header] -> Parser (Maybe Int)
    lookupContentLength = maybe (return Nothing) readContentLength . findContentLength
      where
        findContentLength :: [Header] -> Maybe Header
        findContentLength = find $ (ContentLength ==) . fieldName
        readContentLength t = case decimal $ fieldValue t of
            (Left _) -> fail "cant read content length"
            (Right (cl, "")) -> return $ Just cl
            -- No impnput is consumed ... that mean some thing is wrong :)
            (Right (_, _)) -> fail "cant read content length"
