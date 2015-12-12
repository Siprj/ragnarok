{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       TestCase.Network.SIP.Type
-- Description:  Tests of SIP types
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Stability:    stable
-- Portability:  NoImplicitPrelude, OverloadedStrings
--
-- Unit and property tests for SIP types and their instances.
module TestCase.Network.SIP.Parser.Header (tests)
  where

import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Data.Either (Either(Right))
import Data.Function (($))
import Data.Text (Text)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit.Base ((@=?))
import Test.HUnit.Lang (Assertion)

import Network.SIP.Type.Header
    ( Header(Header)
    , HeaderName
        ( Accept
        , AcceptEncoding
        , Allow
        )
    )
import Network.SIP.Parser.Header (headerParser)

testHeaderParser :: ByteString -> HeaderName -> Text -> Assertion
testHeaderParser s hn ht = Right (Header hn ht) @=? parseOnly headerParser s

tests :: [Test]
tests =
    [ testGroup "Basic header parser unit tests"
        [ testCase
            "header name and one space"
                $ testHeaderParser "Accept: foo bar\r\n" Accept "foo bar"
        , testCase
            "header name and multiple spaces"
                $ testHeaderParser "Accept:    foo bar\r\n" Accept "foo bar"
        , testCase
            "header name with disgusting cases"
                $ testHeaderParser "acCePt: foo bar\r\n" Accept "foo bar"
        , testCase
            "header name with disgusting cases"
                $ testHeaderParser "acCePt: foo bar\r\n" Accept "foo bar"
        , testCase
            "some another header name"
                $ testHeaderParser "Allow: foo bar\r\n" Allow "foo bar"
        , testCase
            "some another header name with dash"
                $ testHeaderParser "Accept-Encoding: foo bar\r\n"
                    AcceptEncoding "foo bar"
        ]
    ]
