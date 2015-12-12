{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Network.SIP.Parser.Header
-- Description:
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Big description.
module Network.SIP.Parser.Header
    ( headerParser
    )
  where

import Control.Applicative ((<*), (<*>), (*>), pure)
import Data.Attoparsec.ByteString (Parser, manyTill, choice)
import Data.Attoparsec.ByteString.Char8
    ( anyChar
    , char
    , isSpace
    , string
    , stringCI
    , takeWhile
    )
import Data.Functor ((<$>), fmap)
import Data.Function (($))
import Data.Text (pack)

import Network.SIP.Type.Header (Header(Header), HeaderName, headerNameMap)

-- | TODO: Add parser for custom constructor.
-- | TODO: There is lots of back tracking here so it needs to be rewritten.
headerParser :: Parser Header
headerParser = choice $ fmap headerParser' headerNames
  where
    headerParser' p = Header
        <$> p <* char ':'
        <*> (fmap pack $ takeWhile isSpace *> manyTill anyChar (string "\r\n"))

headerNames :: [Parser HeaderName]
headerNames = fmap toParser headerNameMap
  where
    toParser (x, y) = stringCI y *> pure x
