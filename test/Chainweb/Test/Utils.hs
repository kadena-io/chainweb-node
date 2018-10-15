{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module: Chainweb.Test.Utils
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.Utils
(
-- * ChainDb Generation
  toyChainDB
, withDB
, insertN

-- * Toy Server Interaction
, withServer

-- * QuickCheck Properties
, prop_iso
, prop_iso'
, prop_encodeDecodeRoundtrip

-- * Expectations
, assertExpectation
) where

import Control.Concurrent.Async (async, link, uninterruptibleCancel)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.IO.Class

import Data.Bifunctor
import Data.Bytes.Get
import Data.Bytes.Put
import Data.DiGraph (singleton)
import Data.Foldable (foldlM)
import qualified Data.Text as T

import qualified Network.HTTP.Client as HTTP
import Network.Socket (close)
import qualified Network.Wai.Handler.Warp as W

import Servant.Client (ClientEnv, mkClientEnv, BaseUrl(..), Scheme(..))

import Test.QuickCheck
import Test.Tasty.HUnit

import UnliftIO.Exception (bracket)

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..), genesisBlockHeader, testBlockHeaders)
import qualified Chainweb.ChainDB as DB
import Chainweb.ChainId (ChainId)
import Chainweb.Graph (toChainGraph)
import Chainweb.RestAPI (chainwebApplication)
import Chainweb.Version (ChainwebVersion(..))
import Chainweb.Utils

-- -------------------------------------------------------------------------- --
-- ChainDb Generation

-- | Initialize an length-1 `ChainDb` for testing purposes.
--
-- Borrowed from TrivialSync.hs
--
toyChainDB :: ChainId -> IO (BlockHeader, DB.ChainDb)
toyChainDB cid = (genesis,) <$> DB.initChainDb (DB.Configuration genesis)
  where
    graph = toChainGraph (const cid) singleton
    genesis = genesisBlockHeader Test graph cid

-- | Given a function that accepts a Genesis Block and
-- an initialized `DB.ChainDb`, perform some action
-- and cleanly close the DB.
--
withDB :: ChainId -> (BlockHeader -> DB.ChainDb -> IO ()) -> IO ()
withDB cid = bracket (toyChainDB cid) (DB.closeChainDb . snd) . uncurry

-- | Populate a `DB.ChainDb` with /n/ generated `BlockHeader`s.
--
insertN :: Int -> BlockHeader -> DB.ChainDb -> IO DB.Snapshot
insertN n g db = do
    ss <- DB.snapshot db
    let bhs = map DB.entry . take n $ testBlockHeaders g
    foldlM (\ss' bh -> DB.insert bh ss') ss bhs >>= DB.syncSnapshot

-- -------------------------------------------------------------------------- --
-- Toy Server Interaction

-- | Spawn a server that acts as a peer node for the purpose of querying / syncing.
--
withServer :: [(ChainId, DB.ChainDb)] -> (ClientEnv -> IO ()) -> IO ()
withServer chains f = bracket start stop (\(_, _, env) -> f env)
  where
    start = do
        (port, sock) <- W.openFreePort
        readyVar <- newEmptyMVar
        server <- async $ do
            let settings = W.setBeforeMainLoop (putMVar readyVar ()) W.defaultSettings
            W.runSettingsSocket settings sock (chainwebApplication Test chains)
        link server
        mgr <- HTTP.newManager HTTP.defaultManagerSettings
        _ <- takeMVar readyVar
        pure (server, sock, mkClientEnv mgr (BaseUrl Http "localhost" port ""))

    stop (server, sock, _) = do
        uninterruptibleCancel server
        close sock

-- -------------------------------------------------------------------------- --
-- Isomorphisms and Roundtrips

prop_iso :: Eq a => Show a => (b -> a) -> (a -> b) -> a -> Property
prop_iso d e a = a === d (e a)

prop_iso'
    :: Show e
    => Eq a
    => Show a
    => (b -> Either e a)
    -> (a -> b)
    -> a
    -> Property
prop_iso' d e a = Right a === first show (d (e a))

prop_encodeDecodeRoundtrip
    :: Eq a
    => Show a
    => (forall m . MonadGet m => m a)
    -> (forall m . MonadPut m => a -> m ())
    -> a
    -> Property
prop_encodeDecodeRoundtrip d e = prop_iso' (runGetEither d) (runPutS . e)

-- -------------------------------------------------------------------------- --
-- Expectations

assertExpectation
    :: MonadIO m
    => Eq a
    => Show a
    => T.Text
    -> Expected a
    -> Actual a
    -> m ()
assertExpectation msg expected actual = liftIO $ assertBool
    (T.unpack $ unexpectedMsg msg expected actual)
    (getExpected expected == getActual actual)
