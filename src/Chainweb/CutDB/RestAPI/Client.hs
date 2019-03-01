{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.CutDB.RestAPI.Client
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.CutDB.RestAPI.Client
( cutGetClient
, cutPutClient
) where

import Servant.API (NoContent(..))
import Servant.Client

-- internal modules

import Chainweb.ChainId
import Chainweb.Cut.CutHashes
import Chainweb.CutDB.RestAPI
import Chainweb.Version

import Data.Singletons

-- -------------------------------------------------------------------------- --
-- GET Cut Client

cutGetClient
    :: ChainwebVersion
    -> ClientM CutHashes
cutGetClient (FromSing (SChainwebVersion :: Sing v)) = client $ cutGetApi @v

-- -------------------------------------------------------------------------- --
-- PUT Cut Client

cutPutClient
    :: ChainwebVersion
    -> CutHashes
    -> ClientM NoContent
cutPutClient (FromSing (SChainwebVersion :: Sing v)) = client $ cutPutApi @v
