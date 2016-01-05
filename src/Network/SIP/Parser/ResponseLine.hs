{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module:       Network.SIP.Parser.ResponseLine
-- Description:
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Big description.
module Network.SIP.Parser.ResponseLine
    ( firstLineParser
    )
  where

import Control.Applicative ((<*))
import Control.Monad ((>>=), return, fail)
import Data.Attoparsec.ByteString.Char8 (char, decimal)
import Data.Attoparsec.ByteString (Parser, takeByteString)
import Data.Bool ((&&), otherwise)
import Data.Function (($))
import Data.Functor (fmap)
import Data.List (lookup)
import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Data.Ord ((<), (>=))
import Data.Text.Encoding (decodeUtf8)
import Data.Tuple (fst, snd)
import Text.Show (show)

import Network.SIP.Parser.SipVersion (sipVersionParser)
import Network.SIP.Type.ResponseStatus
    ( Status(Status)
    , ResponseCode(Unknown)
    , UnknownResponseCode
        ( Unknown_1xx
        , Unknown_2xx
        , Unknown_3xx
        , Unknown_4xx
        , Unknown_5xx
        , Unknown_6xx
        )
    , responseStatusMap
    )

firstLineParser :: Parser Status
firstLineParser = do
    _ <- sipVersionParser <* char ' '
    code <- (decimal <* char ' ') >>= typeStatusCode
    statusMsg <- fmap decodeUtf8 takeByteString
    return $ Status code statusMsg
  where
    typeStatusCode c =
        maybe (unknownStatusCode c) return $ lookup c $ fmap (\x -> (fst $ snd x, fst x)) responseStatusMap
    unknownStatusCode c
      | c >= 100 && c < 200 = return $ Unknown Unknown_1xx
      | c < 300 = return $ Unknown Unknown_2xx
      | c < 400 = return $ Unknown Unknown_3xx
      | c < 500 = return $ Unknown Unknown_4xx
      | c < 600 = return $ Unknown Unknown_5xx
      | c < 700 = return $ Unknown Unknown_6xx
      | otherwise =
          fail $ "Status code must be bettwen <100 - 699>, but this one is: "
              <> show c
