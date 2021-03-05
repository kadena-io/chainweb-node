-- |
-- Module: Chainweb.BlockHeaderDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A TreeDB for all blocks of a single chain of a Chainweb.
--
module Chainweb.BlockHeaderDB
(
-- * Chain Database Handle
  Configuration(..)
, BlockHeaderDb
, initBlockHeaderDb
, closeBlockHeaderDb
, withBlockHeaderDb
) where

-- internal imports

import Chainweb.BlockHeaderDB.Internal
