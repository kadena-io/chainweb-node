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
import qualified Chainweb.VerifierPlugin.Hyperlane.Message.After224 as After224
import qualified Chainweb.VerifierPlugin.Hyperlane.Message.Before224 as Before224

plugin :: VerifierPlugin
plugin = VerifierPlugin $ \(v, cid, bh) proof caps gasRef ->
  if chainweb224Pact v cid bh
  then After224.runPlugin proof caps gasRef
  else Before224.runPlugin proof caps gasRef