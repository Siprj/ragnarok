{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module:       Network.SIP.LowLevel.Type
-- Description:  Types for low level parser
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Most of these types ware taken from warp package
-- https://github.com/yesodweb/wai/blob/master/warp/Network/Wai/Handler/Warp/Types.hs
module Network.SIP.LowLevel.Type
    ( Header
    , Source(..)
    , InvalidMessage(..)
    , mkSource
    , readSource
    , readSource'
    , leftoverSource
    , readLeftoverSource
    )
  where

import Control.Exception (Exception)
import Control.Monad (return)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Data.Typeable (Typeable)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Data.Eq (Eq)
import Data.Monoid ((<>))
import Text.Show (Show, show)
import GHC.Base (($!))
import Data.String (String)
import qualified Data.ByteString as S (empty, null)
import System.IO (IO)

-- | Type for input streaming.
data Source = Source !(IORef ByteString) !(IO ByteString)

mkSource :: IO ByteString -> IO Source
mkSource func = do
    ref <- newIORef S.empty
    return $! Source ref func

readSource :: Source -> IO ByteString
readSource (Source ref func) = do
    bs <- readIORef ref
    if S.null bs
        then func
        else do
            writeIORef ref S.empty
            return bs

-- | Read from a Source, ignoring any leftovers.
readSource' :: Source -> IO ByteString
readSource' (Source _ func) = func

leftoverSource :: Source -> ByteString -> IO ()
leftoverSource (Source ref _) bs = writeIORef ref bs

readLeftoverSource :: Source -> IO ByteString
readLeftoverSource (Source ref _) = readIORef ref

type Header = (CI ByteString, ByteString)

-- | Error types for bad 'SIP message.
data InvalidMessage
    = BadFirstLine String
    | BadProxyHeader String
    | ConnectionClosedByPeer
    | IncompleteHeaders
    | NonHttp
    | NotEnoughLines [String]
    | OverLargeHeader
    | WrongHeader
  deriving (Eq, Typeable)

instance Show InvalidMessage where
    show (NotEnoughLines xs) = "Incomplete request headers, received: "
        <> show xs
    show (BadFirstLine s) = "Invalid first line of message: " <> show s
    show NonHttp = "Request line specified a non-HTTP request"
    show IncompleteHeaders =
        "Request headers did not finish transmission"
    show ConnectionClosedByPeer = "Client closed connection prematurely"
    show OverLargeHeader = "Request headers too large, possible memory\
        \ attack detected."
    show (BadProxyHeader s) = "Invalid PROXY protocol header: "
        <> show s
    show WrongHeader = "Wrong header"

instance Exception InvalidMessage
