{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module:       Network.SIP.Type.Line
-- Description:  Small type representing one untyped line of sip header.
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Small type representing one untyped line of sip header.
-- This type was taken from warp package
-- https://github.com/yesodweb/wai/blob/master/warp/Network/Wai/Handler/Warp/Types.hs
module Network.SIP.Type.Line (Line) where

import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)

type Line = (CI ByteString, ByteString)
