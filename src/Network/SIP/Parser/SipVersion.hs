{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Network.SIP.Parser.SipVersion
-- Description:  Sip version parser.
-- Copyright:    Copyright (c) 2015-2016 Jan Sipr
-- License:      MIT
module Network.SIP.Parser.SipVersion
    ( sipVersionParser
    )
  where

import Data.Attoparsec.ByteString.Char8 (string)
import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)

sipVersionParser :: Parser ByteString
sipVersionParser = string "SIP/2.0"
