{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.CutDB.RestAPI.Server
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.CutDB.RestAPI.Server
(
-- * Handlers
  cutGetHandler
, cutPutHandler

-- * Cut Server
, cutServer

-- * Some Cut Server
, someCutServer

-- * Run server
, serveCutOnPort
) where

import Control.Concurrent.Chan (newChan, writeChan)
import Control.Lens (view)
import Control.Monad.Except

import Data.Aeson (encode, toJSON)
import Data.Binary.Builder (fromByteString, fromLazyByteString)
import Data.Proxy
import Data.Semigroup

import Network.Wai.EventSource (ServerEvent(..), eventSourceAppChan)
import Network.Wai.Handler.Warp hiding (Port)

import Servant.API
import Servant.Server

import qualified Streaming.Prelude as SP

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..), ObjectEncoded(..))
import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.CutDB.RestAPI
import Chainweb.HostAddress
import Chainweb.RestAPI.Utils
import Chainweb.TreeDB (MaxRank(..))
import Chainweb.Utils
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Handlers

cutGetHandler :: CutDb cas -> Maybe MaxRank -> Handler CutHashes
cutGetHandler db Nothing = liftIO $ cutToCutHashes Nothing <$> _cut db
cutGetHandler db (Just (MaxRank (Max mar))) = liftIO $ do
    !c <- _cut db
    !c' <- limitCut (view cutDbWebBlockHeaderDb db) (int mar) c
    return $! cutToCutHashes Nothing c'

cutPutHandler :: CutDb cas -> CutHashes -> Handler NoContent
cutPutHandler db c = NoContent <$ liftIO (addCutHashes db c)

headerStreamHandler :: CutDb cas -> Tagged Handler Application
headerStreamHandler db = Tagged $ \req respond -> do
    chan <- newChan
    void . SP.mapM_ (writeChan chan . f) . SP.concat $ blockDiffStream db
    eventSourceAppChan chan req respond
  where
    f :: BlockHeader -> ServerEvent
    f bh = ServerEvent (Just $ fromByteString "BlockHeader") Nothing
        [ fromLazyByteString . encode . toJSON $ ObjectEncoded bh ]

-- -------------------------------------------------------------------------- --
-- Cut API Server

cutServer
    :: forall cas (v :: ChainwebVersionT)
    . CutDbT cas v
    -> Server (CutApi v)
cutServer (CutDbT db) =
    cutGetHandler db
    :<|> cutPutHandler db
    :<|> headerStreamHandler db

-- -------------------------------------------------------------------------- --
-- Some Cut Server

someCutServerT :: SomeCutDb cas -> SomeServer
someCutServerT (SomeCutDb (db :: CutDbT cas v)) =
    SomeServer (Proxy @(CutApi v)) (cutServer db)

someCutServer :: ChainwebVersion -> CutDb cas -> SomeServer
someCutServer v = someCutServerT . someCutDbVal v

-- -------------------------------------------------------------------------- --
-- Run Server

serveCutOnPort :: Port -> ChainwebVersion -> CutDb cas -> IO ()
serveCutOnPort p v = run (int p) . someServerApplication . someCutServer v
