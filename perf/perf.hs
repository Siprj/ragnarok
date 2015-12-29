{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Main
-- Description:  Performance test for two different SIP parsers
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Stability:    stable
-- Portability:  portable
--
-- Performance test for two different SIP parsers
module Main (main)
  where

import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Control.Monad (return)
import Data.Either (Either(Right))
import Data.Eq ((==))
import Data.Bool (Bool(True, False))
import Data.Function (($), (.))
import Data.Text (Text)
import Data.Monoid ((<>))
import Data.Maybe (Maybe(Nothing, Just))
import System.IO (IO)
import Criterion.Main
    ( defaultMainWith
    , nf
    , nfIO
    , bgroup
    , bench
    , defaultConfig
    )
import Criterion.Types (Config(timeLimit))

import Network.SIP.Type.Header
    ( Header(Header)
    , HeaderName
        ( Accept
        , AcceptEncoding
        , Allow
        , CallID
        , Contact
        , ContentLength
        , CSeq
        , From
        , MaxForwards
        , To
        , Via
        )
    )
import Network.SIP.Parser.Header
import Network.SIP.Type.Request (Request(Request))
import Network.SIP.Type.RequestMethod (RequestMethod(REGISTER))
import Network.SIP.Type.Uri (Uri(Uri), Scheme(SIP))
import Network.SIP.NAParser as NAParser (parseRequest)
import Network.SIP.LLSIP.Type (mkSource)
import Network.SIP.Parser.Request as SIP (parseRequest)

message1 :: ByteString
message1 = "REGISTER sip:ss2.biloxi.example.com SIP/2.0\r\n" <>
    "Via: SIP/2.0/TLS client.biloxi.example.com:5061;branch=z9hG4bKnashds7\r\n" <>
    "Max-Forwards: 70\r\n" <>
    "From: Bob <sips:bob@biloxi.example.com>;tag=a73kszlfl\r\n" <>
    "To: Bob <sips:bob@biloxi.example.com>\r\n" <>
    "Call-ID: 1j9FpLxk3uxtm8tn@biloxi.example.com\r\n" <>
    "CSeq: 1 REGISTER\r\n" <>
    "Contact: <sips:bob@client.biloxi.example.com>\r\n" <>
    "Content-Length: 0\r\n\r\n"


main :: IO ()
main = do
    source1 <- mkSource $ return message1
    defaultMainWith testConfig
        [ bgroup "Attoparsec parser"
            [ bench "message1" $ nf (parseOnly SIP.parseRequest) message1
            ]
        , bgroup "\"Nativ\" parser"
            [ bench "message1" . nfIO $ NAParser.parseRequest source1
            ]
        ]
  where
    testConfig = defaultConfig { timeLimit = 20 }
