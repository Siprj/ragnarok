{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module:       Network.SIP.Parser.SipVersion
-- Description:
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Big description.
module Network.SIP.Parser.SipVersion
    ( sipVersionParser
    )
  where

import Data.Attoparsec.ByteString.Char8 (string)
import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)

sipVersionParser :: Parser ByteString
sipVersionParser = string "SIP/2.0"
