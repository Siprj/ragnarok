{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Network.SIP.Type.HeaderValues
-- Description:
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Big description.
module Network.SIP.Type.HeaderValues where

import Data.Data (Data)
import Data.Eq (Eq)
import Data.Text (Text)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Text.Show (Show)

-- Performance test
import Control.DeepSeq (NFData)

import Network.SIP.Type.RequestMethod (RequestMethod)
import Network.SIP.Type.TransportProtocol (TransportProtocol)
import Network.SIP.Type.Host (Host)
import Network.SIP.Type.Uri (Uri)

-- | Dummy parameter which must by discarded or rewritten in future.
data Param = Param
  deriving (Show, Eq, Data, Generic, NFData)

-- | This may contain different ways of media exchange in future.
-- For now is acceptable only "application/sdp".
data Accept = Accept
  deriving (Show, Eq, Data, Generic, NFData)

-- | For now the only allowed value is "identity".
data AcceptEncoding = AcceptEncoding
  deriving (Show, Eq, Data, Generic, NFData)

data AccepLanguage = AcceptLanguage [Text]
  deriving (Show, Eq, Data, Generic, NFData)

-- | Weird feature which can be a security risc. See RFC3261 for more info.
data AlertInfo = AlertInfo Text
  deriving (Show, Eq, Data, Generic, NFData)

-- | Identify which method are supported by UA.
data Allow = Allow [RequestMethod]
  deriving (Show, Eq, Data, Generic, NFData)

-- | Header for HTTP Digest authentication.
data AuthenticationInfo = AuthenticationInfo Text
  deriving (Show, Eq, Data, Generic, NFData)

-- | Also used for HTTP Digest authentication
data Authorization = Authorization Text
  deriving (Show, Eq, Data, Generic, NFData)

-- | Call id is unique call identifier through space and time.
data CallID = CallID Text
  deriving (Show, Eq, Data, Generic, NFData)

-- | Additional information about callee or caller
data CallInfo = CallInfo [Text]
  deriving (Show, Eq, Data, Generic, NFData)

-- | See https://tools.ietf.org/html/rfc3261#page-167 for mor details.
data Contact = Contact
    { cDisplayName :: Text
    , cUri :: Uri
    , cParams :: [Param]
    }
  deriving (Show, Eq, Data, Generic, NFData)

-- | Only one content type is supported for now: "application/sdp"
data ContentType = ContentType

-- | CSeq takes word32 (Request sequence number) and request method which
-- started sip transaction see https://tools.ietf.org/html/rfc3261#page-38 for
-- more details.
data CSeq = CSeq Word32 RequestMethod
  deriving (Show, Eq, Data, Generic, NFData)

data From = From
    { fDisplayName :: Text
    , fUri :: Uri
    , fTag :: Text
    , fParams :: [Param]
    }
  deriving (Show, Eq, Data, Generic, NFData)

data MaxForwards = Word8
  deriving (Show, Eq, Data, Generic, NFData)

data To = To
    { tDisplayName :: Text
    , tUri :: Uri
    }
  deriving (Show, Eq, Data, Generic, NFData)

-- | Via is used for SIP response routing back to request sender.
-- Record branch is used as Transaction identifier. But the branch will change
-- through SIP dialog.
data Via = Via
    { vProtocol :: TransportProtocol
    , vHost :: Host
    , vBranch :: Param
    , vParameters :: [Param]
    }
  deriving (Show, Eq, Data, Generic, NFData)
