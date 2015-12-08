{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module:       Network.SIP.Type.Header
-- Description:
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Big description.
module Network.SIP.Type.Header
    ( HeaderField(..)
    , HeaderName(..)
    , headerNameMap
    )
  where

import Data.CaseInsensitive (CI)
import Data.Data (Data)
import Data.Either (Either(Right, Left))
import Data.Eq (Eq)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Show (Show)

data HeaderField = HeaderField
    { fieldName :: Text
    , fieldValue :: Text
    }
  deriving (Show, Eq)

--instance ToSip HeaderField where
--    toSip v = fieldName v <> ": " <> fieldValue v
--
--instance FromSip HeaderField where
--    fromSip t = aEToSipE $ parseOnly headerParser t
--      where
--        headerParser = HeaderField
--            <$> (takeTill (== ':') <* char ':' <* (skipMany $ char ' '))
--            <*> takeText

data HeaderName
    = Accept
    | AcceptEncoding
    | AcceptLanguage
    | AlertInfo
    | Allow
    | AuthenticationInfo
    | Authorization
    | CallID
    | CallInfo
    | Contact
    | ContentDisposition
    | ContentEncoding
    | ContentLanguage
    | ContentLength
    | ContentType
    | CSeq
    | Date
    | ErrorInfo
    | Expires
    | From
    | InReplyTo
    | MaxForwards
    | MIMEVersion
    | MinExpires
    | Organization
    | Priority
    | ProxyAuthenticate
    | ProxyAuthorization
    | ProxyRequire
    | RecordRoute
    | ReplyTo
    | Require
    | RetryAfter
    | Route
    | Server
    | Subject
    | Supported
    | Timestamp
    | To
    | Unsupported
    | UserAgent
    | Via
    | Warning
    | WWWAuthenticate
    | Custom Text
  deriving (Show, Eq)

--instance ToSip HeaderName where
--    toSip (Custom name) = name
--    toSip a = original . fromJust $ lookup a headerNameMap
--
--instance FromSip HeaderName where
--    fromSip t = case filter (\x -> snd x == mk t) headerNameMap of
--        [] -> if "X-" `isPrefixOf` t then
--                Right (Custom $ t)
--                else Left errorMsg
--        h:_ -> Right $ fst h
--      where
--        errorMsg = "Can't parse Header Name: " <> t

headerNameMap :: [(HeaderName, CI Text)]
headerNameMap =
    [ (Accept, "Accept")
    , (AcceptEncoding, "Accept-Encoding")
    , (AcceptLanguage, "Accept-Language")
    , (AlertInfo, "Alert-Info")
    , (Allow, "Allow")
    , (AuthenticationInfo, "Authentication-Info")
    , (Authorization, "Authorization")
    , (CallID, "Call-ID")
    , (CallInfo, "Call-Info")
    , (Contact, "Contact")
    , (ContentDisposition, "Content-Disposition")
    , (ContentEncoding, "Content-Encoding")
    , (ContentLanguage, "Content-Language")
    , (ContentLength, "Content-Length")
    , (ContentType, "Content-Type")
    , (CSeq, "CSeq")
    , (Date, "Date")
    , (ErrorInfo, "Error-Info")
    , (Expires, "Expires")
    , (From, "From")
    , (InReplyTo, "In-Reply-To")
    , (MaxForwards, "Max-Forwards")
    , (MIMEVersion, "MIME-Version")
    , (MinExpires, "Min-Expires")
    , (Organization, "Organization")
    , (Priority, "Priority")
    , (ProxyAuthenticate, "Proxy-Authenticate")
    , (ProxyAuthorization, "Proxy-Authorization")
    , (ProxyRequire, "Proxy-Require")
    , (RecordRoute, "Record-Route")
    , (ReplyTo, "Reply-To")
    , (Require, "Require")
    , (RetryAfter, "Retry-After")
    , (Route, "Route")
    , (Server, "Server")
    , (Subject, "Subject")
    , (Supported, "Supported")
    , (Timestamp, "Timestamp")
    , (To, "To")
    , (Unsupported, "Unsupported")
    , (UserAgent, "User-Agent")
    , (Via, "Via")
    , (Warning, "Warning")
    , (WWWAuthenticate, "WWW-Authenticate")
    ]
