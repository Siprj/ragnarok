{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       TestCase.Network.SIP.Parser.Line
-- Description:  Test of SIP line parser.
-- Copyright:    Copyright (c) 2015-2016 Jan Sipr
-- License:      MIT
module TestCase.Network.SIP.Parser.Line (tests)
  where

import Control.Monad (return, unless)
import Data.ByteString (ByteString)
import Data.Eq ((==))
import Data.Function (($))
import Data.Monoid ((<>))
import Text.Show (show)

import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (Test, testGroup)
import Test.HUnit.Base (assertFailure)
import Test.HUnit.Lang (Assertion)

import Network.SIP.Parser.Line (headerLines)
import Network.SIP.Type.Source (mkSource)

testHeaderLines :: ByteString -> [ByteString] -> Assertion
testHeaderLines s hl = do
    source <- mkSource $ return s
    res <- headerLines source
    unless (res == hl) $ assertFailure $ "Error parsing: " <> show s
        <> " expected result" <> show hl <> "\nActual result: " <> show res

message1 :: ByteString
message1 = "REGISTER sips:ss2.biloxi.example.com SIP/2.0\r\n" <>
    "Via: SIP/2.0/TLS client.biloxi.example.com:5061;branch=z9hG4bKnashds7\r\n" <>
    "Max-Forwards: 70\r\n" <>
    "From: Bob <sips:bob@biloxi.example.com>;tag=a73kszlfl\r\n" <>
    "To: Bob <sips:bob@biloxi.example.com>\r\n" <>
    "Call-ID: 1j9FpLxk3uxtm8tn@biloxi.example.com\r\n" <>
    "CSeq: 1 REGISTER\r\n" <>
    "Contact: <sips:bob@client.biloxi.example.com>\r\n" <>
    "Content-Length: 0\r\n\r\n"

parsedList1 :: [ByteString]
parsedList1 =
    [ "REGISTER sips:ss2.biloxi.example.com SIP/2.0"
    , "Via: SIP/2.0/TLS client.biloxi.example.com:5061;branch=z9hG4bKnashds7"
    , "Max-Forwards: 70"
    , "From: Bob <sips:bob@biloxi.example.com>;tag=a73kszlfl"
    , "To: Bob <sips:bob@biloxi.example.com>"
    , "Call-ID: 1j9FpLxk3uxtm8tn@biloxi.example.com"
    , "CSeq: 1 REGISTER"
    , "Contact: <sips:bob@client.biloxi.example.com>"
    , "Content-Length: 0"
    ]

tests :: [Test]
tests =
    [ testGroup "parse header to lines"
        [ testCase
            "message 1"
                $ testHeaderLines message1 parsedList1
        ]
    ]
