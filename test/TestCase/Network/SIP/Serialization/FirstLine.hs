{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:       TestCase.Network.SIP.Serialization.FirstLine
-- Description:  Test of the SIP first line serialization.
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
module TestCase.Network.SIP.Serialization.FirstLine (tests)
  where

import Data.ByteString (ByteString)
import Data.Function (($), (.))
import Data.Maybe (Maybe(Nothing))

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit.Base ((@=?))
import Test.HUnit.Base (Assertion)

import Network.SIP.Serialization.FirstLine (serializeFirstLine)
import Network.SIP.Type.Message (MessageType(Request, Response))
import Network.SIP.Type.RequestMethod (RequestMethod(INVITE, REGISTER))
import Network.SIP.Type.ResponseStatus
    ( ResponseCode
        ( OK_200
        , Ringing_180
        )
    , Status(Status)
    )
import Network.SIP.Type.Uri (Uri(Uri), Scheme(SIP, SIPS))

testRequest :: ByteString -> RequestMethod -> Uri -> Assertion
testRequest res rq uri = res @=? serializeFirstLine (Request rq uri)

testResponse :: ByteString -> ResponseCode -> Assertion
testResponse res status =
    res @=? serializeFirstLine (Response $ Status status "")

tests :: [Test]
tests =
    [ testGroup "SIP first line serialization"
        [ testCase "Request first line with INVITE"
            . testRequest "INVITE sip:10.10.10.10 SIP/2.0"
                INVITE $ Uri SIP Nothing "10.10.10.10" Nothing
        , testCase "Request first line with REGISTER"
            . testRequest "REGISTER sips:ss2.biloxi.example.com SIP/2.0"
                REGISTER $ Uri SIPS Nothing "ss2.biloxi.example.com" Nothing
        , testCase "Response first line with 200"
            $ testResponse "SIP/2.0 200 OK" OK_200
        , testCase "Response first line with 180"
            $ testResponse "SIP/2.0 180 Ringing" Ringing_180
        ]
    ]
