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

import Control.Lens (set, view)
import Control.Monad.Except

import Data.Proxy
import Data.Semigroup

import qualified Network.Socket as N
import Network.Wai.Handler.Warp hiding (Port)

import Servant.API
import Servant.Server

-- internal modules

import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.CutDB.RestAPI
import Chainweb.HostAddress
import Chainweb.RestAPI.Utils
import Chainweb.TreeDB (MaxRank(..))
import Chainweb.Utils
import Chainweb.Version

import P2P.Peer

-- -------------------------------------------------------------------------- --
-- Handlers

cutGetHandler :: CutDb cas -> Maybe MaxRank -> Handler CutHashes
cutGetHandler db Nothing = liftIO $ cutToCutHashes Nothing <$> _cut db
cutGetHandler db (Just (MaxRank (Max mar))) = liftIO $ do
    !c <- _cut db
    !c' <- limitCut (view cutDbWebBlockHeaderDb db) (int mar) c
    return $! cutToCutHashes Nothing c'

cutPutHandler
    :: CutDb cas
    -> CutHashes
    -> N.SockAddr
    -> Handler NoContent
cutPutHandler db c sockAddr = NoContent <$ liftIO (addCutHashes db c')
  where
    -- Fake origin information. The peer id and port are not correct, we
    -- use it ony for rate limiting by IP in the CutDB.
    --
    -- TODO: do proper TLS client authentication
    --
    c' = case _cutOrigin c of
        Just _ -> c
        Nothing -> case sockAddrToHostAddress sockAddr of
            Nothing -> c
            Just ha ->
                let pinfo = PeerInfo
                        { _peerAddr = ha
                        , _peerId = Nothing
                        }
                in set cutOrigin (Just pinfo) c

-- -------------------------------------------------------------------------- --
-- Cut API Server

cutServer
    :: forall cas (v :: ChainwebVersionT)
    . CutDbT cas v
    -> Server (CutApi v)
cutServer (CutDbT db) = cutGetHandler db :<|> cutPutHandler db

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
