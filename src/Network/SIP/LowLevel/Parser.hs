{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module:       Network.SIP.LowLevel.Parser
-- Description:  Low level parser
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Low level parser taken form warp package
-- https://github.com/yesodweb/wai/blob/master/warp/Network/Wai/Handler/Warp/Request.hs
module Network.SIP.LowLevel.Parser
    ( headerLines
    , parseHeader
    )
  where

import Control.Exception (throwIO)
import Control.Monad (return)
import Control.Monad (when)
import Data.Bool (Bool(True, False), (||), (&&), not, otherwise)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
    ( append
    , break
    , drop
    , dropWhile
    , elemIndex
    , index
    , length
    , null
    )
import qualified Data.ByteString.Unsafe as SU (unsafeTake, unsafeDrop)
import Data.CaseInsensitive (mk)
import Data.Eq ((==))
import Data.Function (($), (.), id)
import Data.Int (Int)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Ord ((<), (>))
import Prelude ((+), (-))
import System.IO (IO)

import Network.SIP.LowLevel.Type
    ( Header
    , InvalidRequest
        ( ConnectionClosedByPeer
        , IncompleteHeaders
        , OverLargeHeader
        )
    , Source
    , readSource
    , readSource'
    , leftoverSource
    )


-- | Acording to rfc3261 SIP message length MUST NOT be greater than UDP
-- packet.
maxTotalHeaderLength :: Int
maxTotalHeaderLength = 65507

headerLines :: Source -> IO [ByteString]
headerLines src = do
    bs <- readSource src
    if S.null bs
        then throwIO ConnectionClosedByPeer
        else push src (THStatus 0 id id) bs


parseHeader :: ByteString -> Header
parseHeader s =
    let (k, rest) = S.break (== 58) s -- ':'
        rest' = S.dropWhile (\c -> c == 32 || c == 9) $ S.drop 1 rest
     in (mk k, rest')


----------------------------------------------------------------

type BSEndo = ByteString -> ByteString
type BSEndoList = [ByteString] -> [ByteString]

data THStatus = THStatus
    {-# UNPACK #-} !Int -- running total byte count
    BSEndoList -- previously parsed lines
    BSEndo -- bytestrings to be prepended

----------------------------------------------------------------

push :: Source -> THStatus -> ByteString -> IO [ByteString]
push src (THStatus len lines prepend) bs'
        -- Too many bytes
        | len > maxTotalHeaderLength = throwIO OverLargeHeader
        | otherwise = push' mnl
  where
    bs = prepend bs'
    bsLen = S.length bs
    mnl = do
        nl <- S.elemIndex 10 bs
        -- check if there are two more bytes in the bs
        -- if so, see if the second of those is a horizontal space
        if bsLen > nl + 1 then
            let c = S.index bs (nl + 1)
                b = case nl of
                      0 -> True
                      1 -> S.index bs 0 == 13
                      _ -> False
            in Just (nl, not b && (c == 32 || c == 9))
            else
            Just (nl, False)

    {-# INLINE push' #-}
    push' :: Maybe (Int, Bool) -> IO [ByteString]
    -- No newline find in this chunk.  Add it to the prepend,
    -- update the length, and continue processing.
    push' Nothing = do
        bst <- readSource' src
        when (S.null bst) $ throwIO IncompleteHeaders
        push src status bst
      where
        len' = len + bsLen
        prepend' = S.append bs
        status = THStatus len' lines prepend'
    -- Found a newline, but next line continues as a multiline header
    push' (Just (end, True)) = push src status rest
      where
        rest = S.drop (end + 1) bs
        prepend' = S.append (SU.unsafeTake (checkCR bs end) bs)
        len' = len + end
        status = THStatus len' lines prepend'
    -- Found a newline at position end.
    push' (Just (end, False))
      -- leftover
      | S.null line = do
            when (start < bsLen) $ leftoverSource src (SU.unsafeDrop start bs)
            return (lines [])
      -- more headers
      | otherwise   = let len' = len + start
                          lines' = lines . (line:)
                          status = THStatus len' lines' id
                      in if start < bsLen then
                             -- more bytes in this chunk, push again
                             let bs'' = SU.unsafeDrop start bs
                              in push src status bs''
                           else do
                             -- no more bytes in this chunk, ask for more
                             bst <- readSource' src
                             when (S.null bs) $ throwIO IncompleteHeaders
                             push src status bst
      where
        start = end + 1 -- start of next chunk
        line = SU.unsafeTake (checkCR bs end) bs

{-# INLINE checkCR #-}
checkCR :: ByteString -> Int -> Int
checkCR bs pos = if pos > 0 && 13 == S.index bs p then p else pos -- 13 is CR
  where
    !p = pos - 1
