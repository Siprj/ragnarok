{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module:       Network.SIP.Type
-- Description:
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Stability:    stable
-- Portability:  NoImplicitPrelude
--
-- Big description.
module Network.SIP.Type
    ( RequestMethod(..)
    , ResponseCode(..)
    , HeaderField(..)
    , Request(..)
    , Response(..)
    , ToSip(toSip)
    , FromSip(fromSip)
    )
  where

import Prelude (Bounded)
import Prelude (Enum)

import Data.Bool (not)
import Data.CaseInsensitive (CI, original, mk)
import Data.Char (isDigit)
import Data.Data
    ( Data
    , dataTypeOf
    , fromConstr
    , readConstr
    , showConstr
    , toConstr
    )
import Data.Either (Either(Right, Left))
import Data.Eq (Eq, (==))
import Data.Function ((.), ($))
import Data.Functor ((<$>))
import Data.List (map, lookup, filter)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text, dropWhile, pack, unpack, isPrefixOf)
import Data.Tuple (fst, snd, swap)
import GHC.Generics (Generic)
import Network.URI (URI)
import Text.Show (Show)

import Network.SIP.Utils (maybeToEither)

class ToSip a where
    toSip :: a -> Text

class FromSip a where
    fromSip :: Text -> Either Text a

data HeaderField = HeaderField
    { fieldName :: HeaderName
    , fieldValue :: Text
    }
  deriving (Show, Eq)

data RequestMethod
    = ACK
    | BYE
    | CANCEL
    | INFO
    | INVITE
    | MESSAGE
    | NOTIFY
    | OPTIONS
    | PRACK
    | PUBLISH
    | REFER
    | REGISTER
    | SUBSCRIBE
    | UPDATE
  deriving (Show, Eq, Enum, Data, Generic, Bounded)

instance ToSip RequestMethod where
    toSip = pack . showConstr . toConstr

instance FromSip RequestMethod where
    fromSip t = maybeToEither errorMsg $
        fromConstr <$> readConstr (dataTypeOf ACK) (unpack t)
      where
        errorMsg = "Error: Can't parse Request Method: " <> t

data ResponseCode
    = OK_200
    | Ringing_180
    | SessionProgres_183
    | BadRequest_400
    | Unauthorized_401
    | Forbidden_403
  deriving (Show, Eq, Data, Generic)

-- Unknown messages must be moved somewhere else
data UnknownResponseCode
    = Unknown_1xx
    | Unknown_2xx
    | Unknown_3xx
    | Unknown_4xx
    | Unknown_5xx
    | Unknown_6xx
  deriving (Show, Eq)

instance ToSip ResponseCode where
    toSip = dropWhile (not . isDigit) . pack . showConstr . toConstr

instance FromSip ResponseCode where
    fromSip t = maybeToEither errorMsg .
        lookup (Status t "") $ map swap responseStatusMap
      where
        errorMsg = "Cant parse Response Code: " <> t

data Status = Status
    { statusCode :: Text
    , statusMsg :: Text
    }
  deriving (Show)

instance Eq Status where
    a == b =  statusCode a == statusCode b

responseStatusMap :: [(ResponseCode, Status)]
responseStatusMap =
    [ (OK_200, Status "200" "OK")
    , (Ringing_180, Status "180" "Ringing")
    , (SessionProgres_183, Status "183" "Session Progress")
    , (BadRequest_400, Status "400" "Bad Request")
    , (Unauthorized_401, Status "401" "Unauthorized")
    , (Forbidden_403, Status "403" "Forbidden")
    ]

data Request a = Request
    { rqMethod :: RequestMethod
    , rqUri :: URI
    , rqHeaders :: [HeaderField]
    , rqBody :: a
    }
  deriving (Show)

data Response a = Response
    { rspCode :: ResponseCode
    , rspReason :: Text
    , rspHeaders :: [HeaderField]
    , rspBody :: a
    }
  deriving (Show)

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

instance ToSip HeaderName where
    toSip (Custom name) = name
    toSip a = original . fromJust $ lookup a headerNameMap

instance FromSip HeaderName where
    fromSip t = case filter (\x -> snd x == mk t) headerNameMap of
        [] -> if "X-" `isPrefixOf` t then
                Right (Custom $ t)
                else Left errorMsg
        h:_ -> Right $ fst h
      where
        errorMsg = "Can't parse Header Name: " <> t

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
