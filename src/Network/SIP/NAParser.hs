{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module:       Network.SIP.NAParser
-- Description:  Low level parser
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Low level parser taken form warp package
-- https://github.com/yesodweb/wai/blob/master/warp/Network/Wai/Handler/Warp/Request.hs
module Network.SIP.NAParser
    ( parseRequest
    )
  where

import Control.Applicative ((*>), (<*))
import Control.Exception (throwIO)
import Control.Monad (return, when, fail, sequence)
import Data.Attoparsec.ByteString (Parser, parseOnly, string, (<?>))
import Data.Attoparsec.ByteString.Char8 (takeTill)
import Data.Bool (Bool(True, False), (||), (&&), not, otherwise)
import Data.ByteString (ByteString, break, dropWhile, drop)
import qualified Data.ByteString as S
    ( null
    , length
    , index
    , append
    , drop
    , elemIndex
    )
import qualified Data.ByteString.Unsafe as SU (unsafeTake, unsafeDrop)
import Data.CaseInsensitive (CI, mk)
import Data.Eq ((==))
import Data.Either (Either(Right, Left))
import Data.Function (($), (.), id)
import Data.Functor (fmap)
import Data.List (lookup)
import Data.Monoid ((<>))
import Data.Int (Int)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Ord ((<), (>))
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Prelude ((+), (-))
import System.IO (IO)

import Network.SIP.LowLevel.Type
    ( InvalidRequest
        ( ConnectionClosedByPeer
        , IncompleteHeaders
        , OverLargeHeader
        , BadFirstLine
        , WrongHeader
        )
    , Source
    , readSource
    , readSource'
    , leftoverSource
    )
import qualified Network.SIP.LowLevel.Type as LL (Header)
import Network.SIP.LowLevel.Parser (headerLines, parseHeader)
import Network.SIP.Parser.RequestMethod (requestMethodParser)
import Network.SIP.Parser.Uri (parseUri)
import Network.SIP.Type.Header (Header(Header), headerNameMapCI)
import Network.SIP.Type.RequestMethod (RequestMethod)
import Network.SIP.Type.Request (Request(Request))
import Network.SIP.Type.Uri (Uri)

sipVersion :: ByteString
sipVersion = "SIP/2.0"

parseRequest :: Source -> IO Request
parseRequest source = do
    lines <- headerLines source
    (rm, uri, rLines) <- parseFirstLine lines
    hs <- sequence . fmap typeHeader $ fmap parseHeader rLines
    return $ Request rm uri hs Nothing
  where
    parseFirstLine :: [ByteString] -> IO (RequestMethod, Uri, [ByteString])
    parseFirstLine [] = throwIO IncompleteHeaders
    parseFirstLine (x:xs) = do
        case parseOnly firstLineParser x of
            Right (m, u) -> return (m, u, xs)
            Left s -> throwIO $ BadFirstLine s

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

    typeHeader :: LL.Header -> IO Header
    typeHeader (h, v) = maybe (throwIO WrongHeader) return $ fmap (\x -> Header x (decodeUtf8 v)) $ lookup h headerNameMapCI

