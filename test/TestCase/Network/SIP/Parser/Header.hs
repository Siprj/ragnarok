{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       TestCase.Network.SIP.Parser.Header
-- Description:  Test of header parser.
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
module TestCase.Network.SIP.Parser.Header (tests)
  where

import Control.Monad ((>>=))
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Data.Function (($))
import Data.Text (Text)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit.Base ((@=?))
import Test.HUnit.Lang (Assertion)

import Network.SIP.Type.Header
    ( HeaderName
        ( Accept
        , AcceptEncoding
        , Allow
        )
    )
import Network.SIP.Parser (typeHeader)

testHeaderParser :: (CI ByteString, ByteString) -> HeaderName -> Text -> Assertion
testHeaderParser s hn ht = typeHeader s >>=  ((hn, ht) @=?)

tests :: [Test]
tests =
    [ testGroup "Basic header parser unit tests"
        [ testCase
            "header name and one space"
                $ testHeaderParser ("Accept", "foo bar") Accept "foo bar"
        , testCase
            "header name and multiple spaces"
                $ testHeaderParser ("Accept", "foo bar") Accept "foo bar"
        , testCase
            "header name with disgusting cases"
                $ testHeaderParser ("acCePt", "foo bar") Accept "foo bar"
        , testCase
            "header name with disgusting cases"
                $ testHeaderParser ("acCePt", "foo bar") Accept "foo bar"
        , testCase
            "some another header name"
                $ testHeaderParser ("Allow", "foo bar") Allow "foo bar"
        , testCase
            "some another header name with dash"
                $ testHeaderParser ("Accept-Encoding", "foo bar")
                    AcceptEncoding "foo bar"
        ]
    ]
