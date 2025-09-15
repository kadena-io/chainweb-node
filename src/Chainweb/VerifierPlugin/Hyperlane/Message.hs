{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Chainweb.VerifierPlugin.Hyperlane.Message
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
--
-- Verifier plugin for Hyperlane Message.
-- Verifies the message using the provided metadata.
--
module Chainweb.VerifierPlugin.Hyperlane.Message (plugin) where

import Chainweb.VerifierPlugin
import Chainweb.VerifierPlugin.Hyperlane.Message.After225 qualified as After225

plugin :: VerifierPlugin
plugin = VerifierPlugin $ \(cid, bh) proof caps gasRef ->
  After225.runPlugin proof caps gasRef
