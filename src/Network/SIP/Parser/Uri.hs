{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Network.SIP.Parser.URI
-- Description:  Uri parser with sip specific properties.
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Uri from from Network.URI is not able correctly parser sip:name@host format.
-- This parser is here to fill the gap. The RFC3986 is really complicated so
-- I implemented only relevant parts of the RFC.
--
-- I don't remember to see any query or fragments in SIP packets. Use of these
-- part of URI is not described in RFC3261 at all. This is my reason to not
-- implement them in parser.
module Network.SIP.Parser.Uri
    ( parseUri
    )
  where

import Control.Applicative ((<|>), (<*>), (<*))
import Data.Attoparsec.ByteString
    ( Parser
    , endOfInput
    , option
    , parseOnly
    )
import Data.Attoparsec.ByteString.Char8
    ( decimal
    , inClass
    , many1
    , satisfy
    , string
    )
import Data.ByteString (ByteString)
import Data.Either (Either)
import Data.Function ((.), ($), const)
import Data.Functor ((<$>), fmap, void)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Text (pack, Text)

import Network.SIP.Type.Uri (Uri(Uri), Scheme(SIP, SIPS))
import Network.SIP.Utils (aEToSipE)

parseUri :: ByteString -> Either Text Uri
parseUri = aEToSipE . parseOnly uriParser

-- | One uglly parser.
uriParser :: Parser Uri
uriParser = Uri
    <$> schemeParser
    <*> userParser
    <*> hostParser
    <*> portParser
  where
    schemeParser =
        fmap (const SIP) (string "sip:")
        <|> fmap (const SIPS) (string "sips:")

    userParser :: Parser (Maybe Text)
    userParser = option Nothing . fmap (Just . pack) $
    -- TODO: List of characters allowed in user is kind of weird.
    -- It should be reviewed in future.
        many1 (satisfy $ inClass "a-zA-Z0-9;:&=+$,._%") <* string "@"

    hostParser = fmap pack $
        many1 (satisfy $ inClass "a-zA-Z0-9._-") <* hostEndParser

    hostEndParser = void (string ":") <|> endOfInput

    portParser = option Nothing . fmap Just $ decimal
