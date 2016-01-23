{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       TestCase.Network.SIP.Serialization.Header
-- Description:  Test of SIP message serialization.
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
module TestCase.Network.SIP.Serialization.Header (tests)
  where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Function (($))
import Data.Text.Encoding (decodeUtf8)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit.Base ((@=?))
import Test.HUnit.Lang (Assertion)

import Network.SIP.Type.Header
    ( HeaderName
        ( Accept
        , AcceptEncoding
        , Custom
        )
    )
import Network.SIP.Serialization.Header (serializeHeader)

testHeader :: HeaderName -> ByteString -> ByteString-> Assertion
testHeader headerName headerName' value =
    headerName' <> ": " <> value @=?
        serializeHeader (headerName, decodeUtf8 value)

tests :: [Test]
tests =
    [ testGroup "Header serialization tests"
        [ testCase
            "Simple header"
                $ testHeader Accept "Accept" "value"
        , testCase
            "Header with dash"
                $ testHeader AcceptEncoding "Accept-Encoding" "value2 val val"
        , testCase
            "Custom header"
                $ testHeader (Custom "Custom") "X-Custom" "foo bar baz"
        ]
    ]
