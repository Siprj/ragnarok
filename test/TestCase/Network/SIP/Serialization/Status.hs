{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:       TestCase.Network.SIP.Serialization.Status
-- Description:  Test of the response status serialization.
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
module TestCase.Network.SIP.Serialization.Status (tests)
  where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Function (($))

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit.Base ((@=?))

import Network.SIP.Type.ResponseStatus
    ( Status(Status)
    , ResponseCode
        ( OK_200
        , Ringing_180
        )
    )
import Network.SIP.Serialization.Status (serializeStatus)

testStatus :: ByteString -> ResponseCode -> Test
testStatus r code =
    testCase (unpack r) $ r @=? serializeStatus (Status code "")

tests :: [Test]
tests =
    [ testGroup "Response status serialization test"
        [ testStatus "200 OK" OK_200
        , testStatus "180 Ringing" Ringing_180
        ]
    ]
