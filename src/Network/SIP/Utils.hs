{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       Network.SIP.Utils
-- Description:
-- Copyright:    Copyright (c) 2015 Jan Sipr
-- License:      MIT
--
-- Stability:    stable
-- Portability:  NoImplicitPrelude
--
-- Big description.
module Network.SIP.Utils
    ( maybeToEither
    )
  where

import Data.Maybe (Maybe(Just, Nothing))
import Data.Either (Either(Left, Right))

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just a) = Right a
maybeToEither e Nothing = Left e
