{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       Main
-- Description:  Unit tests executor
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Stability:    stable
-- Portability:  portable
--
-- Unit tests executor.
module Main (main)
  where

import System.IO (IO)

import Test.Framework (defaultMain)

import TestCase (tests)

main :: IO ()
main = defaultMain tests
