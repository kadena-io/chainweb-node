-- |
-- Module: Chainweb.Pact.Checkpoint
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental

-- Pact checkpoint module for Chainweb

module Chainweb.Pact.Checkpoint where

import qualified Pact.Interpreter as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Hash as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Runtime as P
import qualified Pact.Types.Server as P

--placeholder for makeCheckpoint
makeCheckpoint :: P.Hash -> Integer -> (PactDbState e) -> P.CommandM p ()
makeCheckpoint _hash _height _pactState = return ()

--placeholder for restoreCheckpoint
restoreCheckpoint :: P.Hash -> Integer -> P.CommandM p (PactDbState e)
restoreCheckpoint _theHash _height = undefined
