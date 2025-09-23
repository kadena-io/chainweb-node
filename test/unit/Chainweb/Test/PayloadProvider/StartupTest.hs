{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Test.PayloadProvider.StartupTest
    ( tests
    ) where

import Chainweb.ChainId
import Chainweb.Graph (singletonChainGraph)
import Chainweb.Logger
import Chainweb.Pact.Backend.ChainwebPactDb qualified as ChainwebPactDb
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.PactService
import Chainweb.Pact.Payload.PayloadStore.InMemory
import Chainweb.Pact.Types
import Chainweb.Test.Pact.Utils
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils
import Chainweb.Version
import Control.Exception (try, displayException)
import Control.Exception.Safe (SomeException)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.List
import Data.Maybe
import Data.Version (Version(..))
import Pact.Types.SQLite
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "PayloadProvider.Startup"
    [ testMinChainwebVersionValidation
    ]

-- | Helper function for setting up tests with loggerand sqlite
withStartupTestSetup
    :: TestName
    -> (GenericLogger -> SQLiteEnv -> IO ())
    -> (HasVersion => GenericLogger -> SQLiteEnv -> IO ())
    -> TestTree
withStartupTestSetup name setup action = withResourceT (withTempChainSqlite cid) $ \sqlIO -> do
    testCase name $ do
        logger <- getTestLogger
        (sql, _sqlReadPool) <- sqlIO

        setup logger sql

        withVersion v $ runResourceT $ do
            liftIO $ action logger sql
  where
    cid = unsafeChainId 0
    v = instantCpmTestVersion singletonChainGraph

-- | Initialize schema for tests
initStartupTestSchema :: GenericLogger -> SQLiteEnv -> IO ()
initStartupTestSchema _logger sql = ChainwebPactDb.initSchema sql

-- | Test that the minimum chainweb version validation works correctly
testMinChainwebVersionValidation :: TestTree
testMinChainwebVersionValidation = withStartupTestSetup "minimum chainweb version validation"
    initStartupTestSchema
    $ \logger sql -> do
        -- Test with no existing version - should succeed
        pdb <- newPayloadDb

        result1 <- try @SomeException $ runResourceT $ withPactService cid Nothing mempty logger Nothing pdb pool sql cfg genesis
        case result1 of
            Left e
                | "PactService required at least version:" `isInfixOf` displayException e ->
                    liftIO $ assertFailure $ "PactService should start successfully when no minimum version is set: " <> displayException e
            _ -> return ()

        let version2 = Version [2, 2] []
        setMinChainwebVersion sql version2

        version2' <- getMinChainwebVersion sql
        liftIO $ assertEqual "Should return the set version" (Just version2) version2'


        result2 <- try @SomeException $ runResourceT $ withPactService cid Nothing mempty logger Nothing pdb pool sql cfg genesis
        case result2 of
            Left e
                | "PactService required at least version:" `isInfixOf` displayException e ->
                    liftIO $ assertFailure $ "PactService should start successfully when no minimum version is set: " <> displayException e
            _ -> return ()

        let version3 = Version [200, 2] []
        setMinChainwebVersion sql version3

        version3' <- getMinChainwebVersion sql
        liftIO $ assertEqual "Should return the set version" (Just version3) version3'

        result3 <- try @SomeException $ runResourceT $ withPactService cid Nothing mempty logger Nothing pdb pool sql cfg genesis
        case result3 of
            Left e
                | "PactService required at least version:" `isInfixOf` displayException e ->
                    pure ()
            _ ->  liftIO $ assertFailure $ "PactService should not start successfully when minimum version is not reached"

  where
    cid = unsafeChainId 0
    cfg = defaultPactServiceConfig
    genesis = GeneratingGenesis
    pool = error "Pool not needed for this test"


-- Helper functions (copied from PactService.hs local where clause)
getMinChainwebVersion :: SQLiteEnv -> IO (Maybe Version)
getMinChainwebVersion sql = qry_ sql "SELECT minMajorVersion, minMinorVersion from ChainwebMeta limit 1" [RInt, RInt] >>= \case
    [[SInt major, SInt minor]] -> pure $ Just $ Version [fromIntegral major, fromIntegral minor] []
    [] -> pure Nothing
    _ -> error "incorrect column types"

setMinChainwebVersion :: SQLiteEnv -> Version -> IO ()
setMinChainwebVersion sql (Version (major:minor:_) _) = do
    mMinVersion <- getMinChainwebVersion sql
    if isJust mMinVersion
        then
            void $ qry sql
                "UPDATE ChainwebMeta \
                \SET minMajorVersion = ?, minMinorVersion = ?"
                [SInt (fromIntegral major), SInt (fromIntegral minor)] [RInt]
        else
            void $ qry sql
                "INSERT INTO ChainwebMeta (minMajorVersion, minMinorVersion) VALUES (?, ?)"
                [SInt (fromIntegral major), SInt (fromIntegral minor)] [RInt]
setMinChainwebVersion _ (Version _ _) =
    error "version formatting does not match"
