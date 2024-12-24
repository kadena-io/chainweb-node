-- |
-- Module: Chainweb.PayloadProvider.EVM.HeaderDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A TreeDB for all blocks of a single chain of a Chainweb.
--
module Chainweb.PayloadProvider.EVM.HeaderDB
(
-- * Chain Database Handle
  Configuration(..)
, HeaderDb
, initHeaderDb
, closeHeaderDb
, withHeaderDb
) where

-- internal imports

import Chainweb.PayloadProvider.EVM.HeaderDB.Internal
