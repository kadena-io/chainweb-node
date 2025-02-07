{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Chainweb.VerifierPlugin.Hyperlane.Message
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
--
-- Verifier plugin for Hyperlane Message.
-- Verifies the message using the provided metadata.
--
module Chainweb.VerifierPlugin.Hyperlane.Message (plugin) where

import Chainweb.Version.Guards
import Chainweb.VerifierPlugin
import qualified Chainweb.VerifierPlugin.Hyperlane.Message.After225 as After225

plugin :: VerifierPlugin
plugin = VerifierPlugin $ \(cid, bh) proof caps gasRef ->
  After225.runPlugin proof caps gasRef
