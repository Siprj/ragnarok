{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module:       Network.SIP.Type.Error
-- Description:  Type used for error exceptions raised from whole SIP.
-- Copyright:    Copyright (c) 2015-2016 Jan Sipr
-- License:      MIT
--
-- Type used for error exceptions raised from whole SIP.
-- This was taken from warp package and slightly modified.
-- https://github.com/yesodweb/wai/blob/master/warp/Network/Wai/Handler/Warp/Types.hs
module Network.SIP.Type.Error where

import Control.Exception (Exception)
import Data.Eq (Eq)
import Data.Monoid ((<>))
import Data.String (String)
import Data.Typeable (Typeable)
import Text.Show (Show, show)

-- | Error types for bad 'SIP message.
data InvalidMessage
    = BadContentLength
    | BadFirstLine String
    | BadProxyHeader String
    | ConnectionClosedByPeer
    | IncompleteHeaders
    | NonSip
    | NotEnoughLines [String]
    | OverLargeHeader
    | WrongHeader
  deriving (Eq, Typeable)

instance Show InvalidMessage where
    show (NotEnoughLines xs) = "Incomplete request headers, received: "
        <> show xs
    show BadContentLength = "Content Length is in wrong format"
    show (BadFirstLine s) = "Invalid first line of message: " <> show s
    show NonSip = "Request/Response line is not regognized; non-SIP message"
    show IncompleteHeaders =
        "Message headers did not finish transmission"
    show ConnectionClosedByPeer = "Client closed connection prematurely"
    show OverLargeHeader = "Message headers too large, possible memory\
        \ attack detected."
    show (BadProxyHeader s) = "Invalid PROXY protocol header: "
        <> show s
    show WrongHeader = "Wrong header"

instance Exception InvalidMessage
