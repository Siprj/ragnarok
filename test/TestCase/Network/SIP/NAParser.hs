{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       TestCase.Network.NAParser
-- Description:
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Stability:    stable
-- Portability:  NoImplicitPrelude, OverloadedStrings
--
module TestCase.Network.SIP.NAParser (tests)
  where

import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Control.Monad (return)
import Data.Either (Either(Right))
import Data.Eq ((==))
import Data.Bool (Bool(True, False))
import Data.Function (($))
import Data.Text (Text)
import Data.Monoid ((<>))
import Data.Maybe (Maybe(Nothing, Just))

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit.Base ((@=?))
import Test.HUnit.Lang (Assertion)
import Test.HUnit.Base (assertFailure)

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
import Network.SIP.NAParser (parseRequest)
import Network.SIP.LLSIP.Type (mkSource)

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

result1 :: Request
result1 = Request
    REGISTER
    (Uri SIP Nothing "ss2.biloxi.example.com" Nothing)
    [ Header Via "SIP/2.0/TLS client.biloxi.example.com:5061;branch=z9hG4bKnashds7"
    , Header MaxForwards "70"
    , Header From "Bob <sips:bob@biloxi.example.com>;tag=a73kszlfl"
    , Header To "Bob <sips:bob@biloxi.example.com>"
    , Header CallID "1j9FpLxk3uxtm8tn@biloxi.example.com"
    , Header CSeq "1 REGISTER"
    , Header Contact "<sips:bob@client.biloxi.example.com>"
    , Header ContentLength "0"
    ]
    Nothing

test :: ByteString -> Request -> Assertion
test s r = do
    source <- mkSource $ return s
    res <- parseRequest source
    case res == r of
        True -> return ()
        False -> assertFailure $ "Result is not eq to expected value"

tests :: [Test]
tests =
    [ testGroup "Testing whole parser"
        [ testCase
            "header name and one space"
                $ test message1 result1
        ]
    ]
