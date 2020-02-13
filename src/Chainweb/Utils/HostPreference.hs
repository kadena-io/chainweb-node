{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Chainweb.Utils.HostPreference
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Utils.HostPreference
(
-- * HostPreference Utils
  hostPreferenceToText
, hostPreferenceFromText
) where

import Control.Monad.Catch (MonadThrow(..))

import Data.Streaming.Network.Internal
import qualified Data.Text as T

import Network.HostAddress

-- internal modules

import Chainweb.Utils.Text

-- -------------------------------------------------------------------------- --
-- Host Preference Utils

hostPreferenceToText :: HostPreference -> T.Text
hostPreferenceToText HostAny = "*"
hostPreferenceToText HostIPv4 = "*4"
hostPreferenceToText HostIPv4Only = "!4"
hostPreferenceToText HostIPv6 = "*6"
hostPreferenceToText HostIPv6Only = "!6"
hostPreferenceToText (Host s) = T.pack s

hostPreferenceFromText :: MonadThrow m => T.Text -> m HostPreference
hostPreferenceFromText "*" = return HostAny
hostPreferenceFromText "*4" = return HostIPv4
hostPreferenceFromText "!4" = return HostIPv4Only
hostPreferenceFromText "*6" = return HostIPv6
hostPreferenceFromText "!6" = return HostIPv6Only
hostPreferenceFromText s = Host . T.unpack . toText <$> hostnameFromText s

-- Orphan instance
--
instance HasTextRepresentation HostPreference where
    toText = hostPreferenceToText
    {-# INLINE toText #-}
    fromText = hostPreferenceFromText
    {-# INLINE fromText #-}

