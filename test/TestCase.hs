{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       TestCase
-- Description:  All test cases aggregated and exported as tests :: [Test].
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Stability:    stable
-- Portability:  NoImplicitPrelude
--
-- All test cases aggregated and exported as @'tests' :: ['Test']@.
module TestCase (tests)
  where

import Test.Framework (Test, testGroup)

import qualified TestCase.Network.SIP.LowLevel.Parser as
    LowLevel.Parser (tests)
import qualified TestCase.Network.SIP.Parser as
    Parser (tests)
import qualified TestCase.Network.SIP.Parser.Header as
    Parser.Header (tests)
import qualified TestCase.Network.SIP.Parser.RequestMethod as
    Parser.RequestMethod (tests)
import qualified TestCase.Network.SIP.Parser.Uri as
    Parser.Uri (tests)
import qualified TestCase.Network.SIP.Serialization.Header as
    Serialization.Header (tests)

tests :: [Test]
tests =
    [ testGroup "TestCase.Network.SIP.LowLevel.Parser" LowLevel.Parser.tests
    , testGroup "TestCase.Network.SIP.Parser.Header" Parser.Header.tests
    , testGroup "TestCase.Network.SIP.Parser" Parser.tests
    , testGroup "TestCase.Network.SIP.Parser.RequestMethod" Parser.RequestMethod.tests
    , testGroup "TestCase.Network.SIP.Parser.Uri" Parser.Uri.tests
    , testGroup "TestCase.Network.SIP.Serialization" Serialization.Header.tests
    ]
