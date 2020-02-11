{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.WebBlockHeaderDB.Types
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.WebBlockHeaderDB.Types
( WebBlockHeaderDb(..)
) where

import qualified Data.HashMap.Strict as HM

-- internal modules

import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Web Chain Database

-- | Every WebChain has the following properties
--
-- * All entires of _webBlockHeaderDb are valid BlockHeaderDbs
-- * There are no dangling adjacent parent hashes
-- * The adjacent hashes of all block headers conform with the chain graph
--   of the web chain.
--
--  TODO: in order to enforce these invariants the insertion to
--  the dbs must be guarded see issue #123.
--
data WebBlockHeaderDb = WebBlockHeaderDb
    { _webBlockHeaderDb :: !(HM.HashMap ChainId BlockHeaderDb)
    , _webChainwebVersion :: !ChainwebVersion
    }

instance HasChainGraph WebBlockHeaderDb where
    _chainGraph = _chainGraph . _webChainwebVersion
    {-# INLINE _chainGraph #-}

instance HasChainwebVersion WebBlockHeaderDb where
    _chainwebVersion = _webChainwebVersion
    {-# INLINE _chainwebVersion #-}
