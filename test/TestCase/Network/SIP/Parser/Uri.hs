{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       TestCase.Network.SIP.Parser.Header
-- Description:  Test of URI parser.
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
module TestCase.Network.SIP.Parser.Uri (tests)
  where

import Control.Applicative (pure)
import Data.ByteString (ByteString)
import Data.Either (Either(Right, Left))
import Data.Function (($))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.Text (Text)
import Text.Show (show)
import Data.Word (Word16)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit.Base ((@=?), assertString)
import Test.HUnit.Lang (Assertion)

import Network.SIP.Type.Uri
    ( Uri(Uri)
    , Scheme(SIP, SIPS)
    )
import Network.SIP.Parser.Uri (parseUri)

testUriParserUser :: ByteString ->  Scheme -> Text -> Text -> Assertion
testUriParserUser s sch u h = Right (Uri sch (Just u) h Nothing) @=? parseUri s

testUriParserAll ::
       ByteString
    -> Scheme
    -> Maybe Text
    -> Text
    -> Maybe Word16
    -> Assertion
testUriParserAll s sch u h p = Right (Uri sch u h p) @=? parseUri s

negativeTest :: ByteString -> Assertion
negativeTest s =
    case parseUri s of
        (Right r) -> assertString $
            "Error: parsing should fail but bytestring: " <> show s
            <> "was parsed like: " <> show r
        (Left _) -> pure ()


tests :: [Test]
tests =
    [ testGroup "Basic uri parsing"
        [ testCase
            "user name and host"
                $ testUriParserUser "sip:john@host.net" SIP "john" "host.net"
        , testCase
            "wierd characters in name"
                $ testUriParserUser "sip:j%$o.hn@host.net" SIP "j%$o.hn"
                    "host.net"
        , testCase
            "host ip address"
                $ testUriParserUser "sip:john@10.0.0.1" SIP "john"
                    "10.0.0.1"
        , testCase
            "another ip address"
                $ testUriParserUser "sip:john@192.169.10.49" SIP "john"
                    "192.169.10.49"
        , testCase
            "ipaddress port"
                $ testUriParserAll "sip:john@192.169.10.49:1234"
                    SIP (Just "john") "192.169.10.49" (Just 1234)
        , testCase
            "no user"
                $ testUriParserAll "sip:192.169.10.49"
                    SIP Nothing "192.169.10.49" Nothing
        , testCase
            "no user and port"
                $ testUriParserAll "sip:192.169.10.49:1234"
                    SIP Nothing "192.169.10.49" (Just 1234)
        , testCase
            "domain name and port"
                $ testUriParserAll "sip:hello.world:1234"
                    SIP Nothing "hello.world" (Just 1234)
        , testCase
            "sips"
                $ testUriParserAll "sips:hello.world:1234"
                    SIPS Nothing "hello.world" (Just 1234)
        ]
    , testGroup "Negative results"
        [ testCase
            "missing username before @"
                $ negativeTest "sip:@hello.world"
        , testCase
            "missing host name"
                $ negativeTest "sip:ahojda@"
        ]
    ]
