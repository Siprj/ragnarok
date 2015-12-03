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
module TestCase.Network.SIP.Type (tests)
  where

import Prelude (minBound, maxBound)

import Data.Bool (Bool(False))
import Data.Eq ((==))
import Data.Either (Either(Left, Right))
import Data.Function (($))

import Test.QuickCheck.Arbitrary (Arbitrary(arbitrary))
import Test.QuickCheck.Gen (elements)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Network.SIP.Type (RequestMethod, fromSip, toSip)

instance Arbitrary RequestMethod where
    arbitrary =
        elements ([minBound..maxBound] :: [RequestMethod])

prop_requestMethodParser :: RequestMethod -> Bool
prop_requestMethodParser r =
    case fromSip $ toSip r of
        Right v ->  r == v
        Left _ -> False


tests :: [Test]
tests =
    [ testGroup "Property tests"
        [ testProperty
            "test methond encoding and decoding" prop_requestMethodParser
        ]
    ]
