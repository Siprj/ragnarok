{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:       TestCase.Network.SIP.Serialization.Uri
-- Description:  Test of the URI serialization.
-- Copyright:    Copyright (c) 2015-2016 Jan Sipr
-- License:      MIT
module TestCase.Network.SIP.Serialization.Uri (tests)
  where

import Data.ByteString (ByteString)
import Data.Function (($), (.))
import Data.Maybe (Maybe(Just, Nothing))

import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (Test, testGroup)
import Test.HUnit.Base ((@=?))
import Test.HUnit.Base (Assertion)

import Network.SIP.Serialization.Uri (serializeUri)
import Network.SIP.Type.Uri (Uri(Uri), Scheme(SIP, SIPS))

testUri :: ByteString -> Uri -> Assertion
testUri r uri = r @=? serializeUri uri

tests :: [Test]
tests =
    [ testGroup "URI serialization tests"
        [ testCase "URI with no user and port"
            . testUri "sip:10.10.10.10" $ Uri SIP Nothing "10.10.10.10" Nothing
        , testCase "URI with user and port"
            . testUri "sips:user@10.10.10.10:1234"
            $ Uri SIPS (Just "user") "10.10.10.10" (Just 1234)
        , testCase "URI with no user and with port"
            . testUri "sips:10.10.10.10:1234"
            $ Uri SIPS Nothing "10.10.10.10" (Just 1234)
        , testCase "URI with user and with no port"
            . testUri "sips:user@10.10.10.10"
            $ Uri SIPS (Just "user") "10.10.10.10" Nothing
        ]
    ]
