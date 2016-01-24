{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       TestCase
-- Description:  All test cases aggregated and exported as tests :: [Test].
-- Copyright:    Copyright (c) 2015-2016 Jan Sipr
-- License:      MIT
--
-- Stability:    stable
-- Portability:  NoImplicitPrelude
--
-- All test cases aggregated and exported as @'tests' :: ['Test']@.
module TestCase (tests)
  where

import Test.Framework (Test, testGroup)

import qualified TestCase.Network.SIP.Parser as Parser (tests)
import qualified TestCase.Network.SIP.Parser.Header as Parser.Header (tests)
import qualified TestCase.Network.SIP.Parser.Line as Parser.Line (tests)
import qualified TestCase.Network.SIP.Parser.RequestMethod as
    Parser.RequestMethod (tests)
import qualified TestCase.Network.SIP.Parser.Uri as Parser.Uri (tests)
import qualified TestCase.Network.SIP.Serialization as Serialization (tests)
import qualified TestCase.Network.SIP.Serialization.FirstLine as
    Serialization.FirstLine (tests)
import qualified TestCase.Network.SIP.Serialization.Header as
    Serialization.Header (tests)
import qualified TestCase.Network.SIP.Serialization.Status as
    Serialization.Status (tests)
import qualified TestCase.Network.SIP.Serialization.Uri as
    Serialization.Uri (tests)

tests :: [Test]
tests =
    [ testGroup "TestCase.Network.SIP.Parser.Header" Parser.Header.tests
    , testGroup "TestCase.Network.SIP.Parser.Line" Parser.Line.tests
    , testGroup "TestCase.Network.SIP.Parser" Parser.tests
    , testGroup "TestCase.Network.SIP.Parser.RequestMethod"
        Parser.RequestMethod.tests
    , testGroup "TestCase.Network.SIP.Parser.Uri" Parser.Uri.tests
    , testGroup "TestCase.Network.SIP.Serialization"
        Serialization.tests
    , testGroup "TestCase.Network.SIP.Serialization.FirstLine"
        Serialization.FirstLine.tests
    , testGroup "TestCase.Network.SIP.Serialization.Header"
        Serialization.Header.tests
    , testGroup "TestCase.Network.SIP.Serialization.Status"
        Serialization.Status.tests
    , testGroup "TestCase.Network.SIP.Serialization.Uri"
        Serialization.Uri.tests
    ]
