{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       Network.SIP.Utils
-- Description:
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Big description.
module Network.SIP.Utils
    ( maybeToEither
    , aEToSipE
    )
  where

import Data.Either (Either(Left, Right), either)
import Data.Function ((.))
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (String)
import Data.Text (Text, pack)

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just a) = Right a
maybeToEither e Nothing = Left e

aEToSipE :: Either String a -> Either Text a
aEToSipE = either (Left . pack) Right
