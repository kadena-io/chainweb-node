{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- due to module import cycle-breaking with pact: pact wants a BlockHeaderDB,
-- but the TreeDB instance wants to know about genesis blocks, which requires
-- validation, which requires pact
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module: Chainweb.BlockHeaderDB
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.BlockHeaderDB
(
-- * Chain Database Handle
  Configuration(..)
, BlockHeaderDb
, initBlockHeaderDb
, closeBlockHeaderDb
, withBlockHeaderDb

-- internal
, seekTreeDb
) where

-- internal imports

import Chainweb.BlockHeaderDB.Internal
