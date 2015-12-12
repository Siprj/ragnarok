{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module:       Network.SIP.Type.RequestMethod
-- Description:
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Big description.
module Network.SIP.Type.RequestMethod
    ( RequestMethod(..)
    )
  where

import Data.Eq (Eq)
import Data.Data (Data)
import Prelude (Enum)
import GHC.Generics (Generic)
import Text.Show (Show)

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
  deriving (Show, Eq, Enum, Data, Generic)

--instance ToSip RequestMethod where
--    toSip = pack . showConstr . toConstr
--
--instance FromSip RequestMethod where
--    fromSip t = maybeToEither errorMsg $
--        fromConstr <$> readConstr (dataTypeOf ACK) (unpack t)
--      where
--        errorMsg = "Error: Can't parse Request Method: " <> t
