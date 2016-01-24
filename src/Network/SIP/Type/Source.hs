{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Network.SIP.Type.Source
-- Description:  Streaming type and its smart constructors.
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Type of the source where the incoming data can be reeded.
-- This type was taken from warp package:
-- https://github.com/yesodweb/wai/blob/master/warp/Network/Wai/Handler/Warp/Types.hs
module Network.SIP.Type.Source
    ( Source
    , mkSource
    , readSource
    , readSource'
    , leftoverSource
    , readLeftoverSource
    )
  where

import Control.Monad (return)
import Data.ByteString (ByteString)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import GHC.Base (($!))
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
leftoverSource (Source ref _) = writeIORef ref

readLeftoverSource :: Source -> IO ByteString
readLeftoverSource (Source ref _) = readIORef ref
