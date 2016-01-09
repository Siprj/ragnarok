{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Network.SIP.Parser
-- Description:
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Big description.
module Network.SIP.Parser
    ( typeHeader
    , parseSipMessage
    )
  where

import Control.Exception (throwIO)
import Control.Monad ((>>=), return, sequence, liftM)
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.Attoparsec.Text as AT (parseOnly, decimal)
import Data.ByteString (ByteString)
import Data.Either (either)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Int (Int)
import Data.List (lookup)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text)
import Data.Tuple (curry, swap)
import System.IO (IO)
import Text.Show (show)

import Network.SIP.LowLevel.Type
    ( InvalidMessage(WrongHeader, BadFirstLine, BadContentLength)
    , Source
    )
import Network.SIP.Type.Header
    ( Header
    , HeaderName(ContentLength)
    , headerNameMap
    )
import Network.SIP.Type.Message (MessageType, Message(Message))
import qualified Network.SIP.LowLevel.Parser as LL (parseHeader)
import qualified Network.SIP.LowLevel.Type as LL (Header)
import Network.SIP.LowLevel.Parser (headerLines, readBody)
import qualified Network.SIP.Parser.RequestLine as Req (firstLineParser)
import qualified Network.SIP.Parser.ResponseLine as Resp (firstLineParser)

typeHeader :: LL.Header -> IO Header
typeHeader (h, v) =
    maybe (throwIO WrongHeader) return $
        fmap (\x -> (x, decodeUtf8 v)) .
            lookup h . fmap swap $ headerNameMap

parseSipMessage :: Source -> IO Message
parseSipMessage src = do
    (mt, bhls) <- headerLines src >>= parseFirstLine
    ths <- sequence . fmap typeHeader $ fmap LL.parseHeader bhls
    body <- maybe (return Nothing) readBody' $ lookup ContentLength ths
    return $ Message mt ths body
  where
    readBody' t = liftM Just (validateContentLength t >>= readBody src)
    validateContentLength :: Text -> IO Int
    validateContentLength =
        either (\_ -> throwIO BadContentLength) return
            . AT.parseOnly AT.decimal

parseFirstLine :: [ByteString] -> IO (MessageType, [ByteString])
parseFirstLine [] = throwIO $ BadFirstLine "Empty message received"
parseFirstLine (x:xs) =
    either (\_ -> throwIO . BadFirstLine $ show x)
        (curry (return . swap) xs)
        $ parseOnly firstLineParser x
  where
    firstLineParser = Resp.firstLineParser <|> Req.firstLineParser
