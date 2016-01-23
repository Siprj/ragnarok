{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       TestCase.Network.SIP.Parser.RequestMethod
-- Description:  Test of request method parser.
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
module TestCase.Network.SIP.Parser.RequestMethod (tests)
  where

import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Data.Bool (Bool(True, False))
import Data.Either (Either(Left, Right))
import Data.Function (($))

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit.Base ((@=?))
import Test.HUnit.Lang (Assertion)

import Network.SIP.Type.RequestMethod
    ( RequestMethod(INVITE, UPDATE)
    )
import Network.SIP.Parser.RequestMethod (requestMethodParser)

testHeaderParser :: ByteString -> RequestMethod -> Assertion
testHeaderParser s r = Right r @=? parseOnly requestMethodParser s

leftToTrue :: Either e a -> Bool
leftToTrue (Left _)  = True
leftToTrue (Right _) = False

testNegativeResult :: ByteString -> Assertion
testNegativeResult s = True @=? leftToTrue (parseOnly requestMethodParser s)

tests :: [Test]
tests =
    [ testGroup "Basic request method parser unit tests"
        [ testCase
            "request method INVITE"
                $ testHeaderParser "INVITE" INVITE
        , testCase
            "request method UPDATE"
                $ testHeaderParser "UPDATE" UPDATE
        , testCase
            "request method UPDATE"
                $ testNegativeResult "UPDAEE"
        , testCase
            "request method UPDATE"
                $ testNegativeResult "ASDFEFFDS"
        ]
    ]
