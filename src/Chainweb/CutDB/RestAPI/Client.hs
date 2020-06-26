{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.CutDB.RestAPI.Client
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Client implementation of the 'Cut' REST API.
--
module Chainweb.CutDB.RestAPI.Client
( cutGetClient
, cutGetClientLimit
, cutPutClient
) where

import Servant.API (NoContent(..))
import Servant.Client

-- internal modules

import Chainweb.ChainId
import Chainweb.Cut.CutHashes
import Chainweb.CutDB.RestAPI
import Chainweb.TreeDB (MaxRank(..))
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- GET Cut Client

cutGetClient
    :: ChainwebVersion
    -> ClientM CutHashes
cutGetClient (FromSingChainwebVersion (SChainwebVersion :: Sing v))
    = client (cutGetApi @v) Nothing

cutGetClientLimit
    :: ChainwebVersion
    -> MaxRank
    -> ClientM CutHashes
cutGetClientLimit (FromSingChainwebVersion (SChainwebVersion :: Sing v))
    = client (cutGetApi @v) . Just

-- -------------------------------------------------------------------------- --
-- PUT Cut Client

cutPutClient
    :: ChainwebVersion
    -> CutHashes
    -> ClientM NoContent
cutPutClient (FromSingChainwebVersion (SChainwebVersion :: Sing v)) = client $ cutPutApi @v
