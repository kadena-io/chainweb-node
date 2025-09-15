{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main(main) where

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.PruneForks qualified as PruneForks
import Chainweb.BlockHeight (BlockHeight (..))
import Chainweb.Core.Brief
import Chainweb.Cut (cutHeaders, unsafeMkCut)
import Chainweb.Cut.Create hiding (join)
import Chainweb.CutDB (cutHashesTable, readHighestCutHeaders)
import Chainweb.Logger
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.PactService qualified as PactService
import Chainweb.Pact.Payload.PayloadStore.RocksDB qualified as Pact.Payload.PayloadStore.RocksDB
import Chainweb.Pact.Types
import Chainweb.Parent
import Chainweb.PayloadProvider (blockHeaderToEvaluationCtx)
import Chainweb.PayloadProvider.Pact
import Chainweb.PayloadProvider.Pact.Genesis (genesisPayload)
import Chainweb.Storage.Table.RocksDB (modernDefaultOptions, withReadOnlyRocksDb, withRocksDb)
import Chainweb.Time
import Chainweb.TreeDB qualified as TreeDB
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Registry
import Chainweb.WebBlockHeaderDB
import Control.Concurrent(threadDelay)
import Control.Concurrent.Async (forConcurrently, forConcurrently_)
import Control.Exception.Safe
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Constraint
import Data.HashSet qualified as HS
import Data.HashMap.Strict qualified as HM
import Data.IORef
import Data.List qualified as List
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.Stack
import Options.Applicative
import Streaming qualified as S
import Streaming.Prelude qualified as S
import System.FilePath ((</>))
import System.LogLevel
import Text.Printf

main :: IO ()
main = join $
    execParser $ info
        (parser <**> helper)
        (fullDesc
            <> progDesc "Replay Pact blocks checking that we get the correct outputs"
            <> header "pact-replay")

getRocksDbDir :: HasCallStack => FilePath -> FilePath
getRocksDbDir base = base </> "0" </> "rocksDb"

getPactDbDir :: HasCallStack => FilePath -> FilePath
getPactDbDir base = base </> "0" </> "sqlite"

isPactChain :: HasVersion => ChainId -> Bool
isPactChain cid = payloadProviderTypeForChain cid == PactProvider

parser :: Parser (IO ())
parser = do
    version <- option (findKnownVersion =<< textReader) (long "chainweb-version" <> short 'v')
    dbDir <- textOption (long "database-directory")
    logLevel <- flag' Debug (long "verbose") <|> flag' Warn (long "quiet") <|> pure Info
    maybeStart <- optional $ BlockHeight <$> textOption (long "start")
    maybeEnd <- optional $ BlockHeight <$> textOption (long "end")
    chains :: Dict HasVersion -> [ChainId] <-
        fmap const (jsonOption (long "chains"))
        <|> pure (\Dict -> filter isPactChain (HS.toList chainIds))
    return $ withVersion version $ do
        -- we'd use a read-only db, but unfortunately we write to rocksdb even
        -- when it's not necessary sometimes during initialization, c.f.
        -- initBlockHeaderDb.
        withRocksDb (getRocksDbDir dbDir) modernDefaultOptions $ \rdb -> do
            -- TODO: use a "real" logger with telemetry backend disabled, to
            -- avoid applyCmdInBlock traces spamming the logs
            let logger = genericLogger logLevel T.putStrLn
            let cutTable = cutHashesTable rdb
            let pdb = Pact.Payload.PayloadStore.RocksDB.newPayloadDb rdb
            let wbhdb = mkWebBlockHeaderDb rdb (tabulateChains (mkBlockHeaderDb rdb))

            initialCut <- unsafeMkCut <$> readHighestCutHeaders (logFunctionText logger) wbhdb cutTable
            limitedCut <- maybe (return initialCut) (\end -> limitCut wbhdb end initialCut) maybeEnd

            failureCount <- fmap sum $ forConcurrently (chains Dict `List.intersect` HM.keys (view cutHeaders limitedCut)) $ \cid -> runResourceT $ do
                let chainLogger = addLabel ("chain", brief cid) logger
                let config = defaultPactServiceConfig

                PactPayloadProvider _ serviceEnv <- withPactPayloadProvider cid rdb Nothing chainLogger Nothing mempty pdb
                    (getPactDbDir dbDir)
                    config
                    (genesisPayload cid)

                bhdb <- getWebBlockHeaderDb wbhdb cid

                let upperEndBlock = limitedCut ^?! cutHeaders . ix cid
                let upper = HS.singleton (TreeDB.UpperBound $ view blockHash upperEndBlock)

                failureCountRef <- liftIO $ newIORef (0 :: Word)
                heightRef <- liftIO $ newIORef (view blockHeight upperEndBlock)
                rateRef <- liftIO $ newIORef (0 :: Double)
                _ <- withAsyncR (logProgress chainLogger cid heightRef rateRef)
                -- Note that branchEntries is descending; so now, we do replays
                -- in descending height order. This works well for us because
                -- replay failures are most likely to happen nearest the tip of
                -- the chains.
                liftIO $ TreeDB.branchEntries bhdb Nothing Nothing Nothing Nothing mempty upper $ \blockStream -> do
                    blockStream
                        & S.takeWhile (\blk -> maybe True (\start -> view blockHeight blk >= start) maybeStart)
                        & withParent

                        & S.mapM (\(h, ph) -> do

                            -- replay the block
                            writeIORef heightRef (view blockHeight h)
                            fmap (h,) $
                                try @_ @SomeException $
                                    PactService.execReadOnlyReplay chainLogger serviceEnv
                                        (view blockPayloadHash h <$ blockHeaderToEvaluationCtx ph))

                        -- Note that this chunking is only used to calculate the
                        -- rate of the replay. The replay still functionally
                        -- proceeds on a single block at a time, because it
                        -- happens *before* the chunking in the pipeline.
                        & S.chunksOf 500
                        & mapsM_ (\blkChunk -> do
                            startTime <- getCurrentTimeIntegral

                            count S.:> (Just lastHdr S.:> x) <- blkChunk
                                & S.mapM (\case
                                    (h, Left err) -> do
                                        atomicModifyIORef' failureCountRef (\n -> (succ n, ()))
                                        logFunctionText chainLogger Error $ "Error block: " <> brief h <> ": " <> sshow err
                                        return h
                                    (h, Right (Just err)) -> do
                                        atomicModifyIORef' failureCountRef (\n -> (succ n, ()))
                                        logFunctionText chainLogger Error $ "Invalid block " <> brief h <> ": " <> sshow err
                                        return h
                                    (h, Right Nothing) -> return h
                                    )
                                & S.copy
                                & S.last
                                & S.length

                            endTime <- getCurrentTimeIntegral
                            let !(TimeSpan (timeTaken :: Micros)) = (endTime `diff` startTime)
                            let !rate :: Double = int count * 1_000_000 / int timeTaken

                            writeIORef rateRef rate

                            return x
                        )

                liftIO $ logFunctionText chainLogger Info $ "finished replaying chain " <> brief cid

                liftIO $ readIORef failureCountRef
            when (failureCount > 0) $
                error $ sshow failureCount <> " blocks failed"
    where
    logProgress logger cid heightRef rateRef = do
        threadDelay 20_000_000
        height <- readIORef heightRef
        rate <- readIORef rateRef
        logFunctionText logger Info $
            "Chain " <> brief cid <>
            " rate " <> T.pack (printf "%.2f" rate) <> "/s"
            <> " at " <> brief height <> " (desc.)"

-- requires that the input is descending
withParent :: Monad m => S.Stream (S.Of h) m r -> S.Stream (S.Of (h, Parent h)) m r
withParent = \strm -> do
    S.lift (S.next strm) >>= \case
        Left r -> return r
        Right (bh, strm') -> go bh strm'
    where
    go bh strm = do
        S.lift (S.next strm) >>= \case
            Left r -> return r
            Right (bh', strm') -> do
                S.yield (bh, Parent bh')
                go bh' strm'

mapsM_ :: Monad m => (forall x. f x -> m x) -> S.Stream f m r -> m r
mapsM_ f = go
    where
    go strm =
        S.inspect strm >>= \case
            Left r -> return r
            Right fstrm -> do
                strm' <- f fstrm
                go strm'
