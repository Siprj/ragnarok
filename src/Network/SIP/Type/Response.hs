{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Network.SIP.Type.Response
-- Description:
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Big description.
module Network.SIP.Type.Response
    ( Response(..)
    )
  where

import Data.ByteString (ByteString)
import Text.Show (Show)

import Network.SIP.Type.ResponseStatus (Status)
import Network.SIP.Type.Header (Header)

data Response = Response
    { rsStatus :: Status
    , rsHeaders :: [Header]
    , rsBody :: ByteString
    }
  deriving (Show)

--instance ToSip Request where
--    toSip v = toSip (rqMethod v) <~> pack (show (rqUri v)) <~> sipVersion <> lineEnd
--        <> (foldl (<>) "" . map (\x -> fieldName x <:> fieldValue x) $ rqHeaders v)
--
