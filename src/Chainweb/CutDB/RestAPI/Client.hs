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
    :: HasVersion
    => ClientM CutHashes
cutGetClient = case implicitVersion of
    FromSingChainwebVersion (SChainwebVersion :: Sing v) ->
        client (cutGetApi @v) Nothing

cutGetClientLimit
    :: HasVersion
    => MaxRank
    -> ClientM CutHashes
cutGetClientLimit = case implicitVersion of
    FromSingChainwebVersion (SChainwebVersion :: Sing v) ->
        client (cutGetApi @v) . Just

-- -------------------------------------------------------------------------- --
-- PUT Cut Client

cutPutClient
    :: HasVersion
    => CutHashes
    -> ClientM NoContent
cutPutClient = case implicitVersion of
    FromSingChainwebVersion (SChainwebVersion :: Sing v) ->
        client $ cutPutApi @v
