{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
module Network.SIP.Type
    ( RequestMethod(..)
    , ResponseCode(..)
    , HeaderField(..)
    , Request(..)
    , Response(..)
    )
  where

import Data.Eq (Eq)
import Data.Text (Text)
import Network.URI (URI)
import Text.Show (Show)


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
  deriving (Show, Eq)

data ResponseCode
    = OK_200
    | Ringing_180
    | SessionProgres_183
    | Unknown_1xx
    | Unknown_2xx
    | Unknown_3xx
    | Unknown_4xx
    | Unknown_5xx
    | Unknown_6xx
  deriving (Show, Eq)

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
  deriving (Show, Eq)
