{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

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
import Control.Concurrent.Async (forConcurrently, forConcurrently_)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Constraint
import Data.HashSet qualified as HS
import Data.IORef
import Data.Text.IO qualified as T
import GHC.Stack
import Options.Applicative
import Streaming qualified as S
import Streaming.Prelude qualified as S
import System.FilePath ((</>))
import System.LogLevel

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
    dbDir <- textOption (long "database-dir")
    logLevel <- flag' Debug (long "verbose") <|> flag' Warn (long "quiet") <|> pure Info
    maybeStart <- optional $ BlockHeight <$> textOption (long "start")
    maybeEnd <- optional $ BlockHeight <$> textOption (long "end")
    progressChunkSize <- textOption (long "progress-chunk-size") <|> pure 500
    chains :: Dict HasVersion -> [ChainId] <-
        fmap const (jsonOption (long "chains"))
        <|> pure (\Dict -> filter isPactChain (HS.toList chainIds))
    return $ withVersion version $ do
        withReadOnlyRocksDb (getRocksDbDir dbDir) modernDefaultOptions $ \rdb -> do
            let logger = genericLogger logLevel T.putStrLn
            let cutTable = cutHashesTable rdb
            let pdb = Pact.Payload.PayloadStore.RocksDB.newPayloadDb rdb
            let wbhdb = mkWebBlockHeaderDb (tabulateChains (mkBlockHeaderDb rdb))

            initialCut <- unsafeMkCut <$> readHighestCutHeaders (logFunctionText logger) wbhdb cutTable
            limitedCut <- maybe (return initialCut) (\end -> limitCut wbhdb end initialCut) maybeEnd

            anyFailed <- fmap or $ forConcurrently (chains Dict) $ \cid -> runResourceT $ do
                readWriteSqlenv <- withSqliteDb cid logger (getPactDbDir dbDir) False
                readOnlySqlPool <- withReadSqlitePool cid (getPactDbDir dbDir)
                let config = defaultPactServiceConfig
                PactPayloadProvider _ serviceEnv <- withPactPayloadProvider cid rdb Nothing logger Nothing mempty pdb
                    (getPactDbDir dbDir)
                    config
                    (genesisPayload cid)

                failedOnChainRef <- liftIO $ newIORef False
                bhdb <- getWebBlockHeaderDb wbhdb cid

                let upperEndBlock = limitedCut ^?! cutHeaders . ix cid
                let upper = (HS.singleton (TreeDB.UpperBound $ view blockHash upperEndBlock))
                liftIO $ TreeDB.branchEntries bhdb Nothing Nothing Nothing Nothing mempty upper $ \blockStream -> do
                    blockStream
                        & S.takeWhile (\blk -> maybe True (\start -> view blockHeight blk >= start) maybeStart)
                        & withParent
                        & S.mapM (\(h, ph) ->
                            (h,) <$> PactService.execReadOnlyReplay logger serviceEnv
                                (view blockPayloadHash h <$ blockHeaderToEvaluationCtx ph))
                        & S.chunksOf progressChunkSize
                        & mapsM_ (\blkChunk -> do
                            startTime <- getCurrentTimeIntegral

                            count S.:> (Just lastHdr S.:> x) <- blkChunk
                                & S.mapM (\case
                                    (h, Just err) -> do
                                        writeIORef failedOnChainRef True
                                        logFunctionText logger Error $ "Invalid block " <> brief h <> ": " <> sshow err
                                        return h
                                    (h, Nothing) -> return h
                                    )
                                & S.copy
                                & S.last
                                & S.length

                            endTime <- getCurrentTimeIntegral
                            let !(TimeSpan (timeTaken :: Micros)) = (endTime `diff` startTime)
                            let speed :: Double = int count * 1_000_000 / int timeTaken

                            logFunctionText logger Info $
                                "Chain " <> brief cid <> " speed " <> sshow speed <> "/s at " <> brief (view blockHeight lastHdr) <> " (desc.)"

                            return x
                        )

                liftIO $ logFunctionText logger Info $ "finished replaying chain " <> brief cid

                liftIO $ readIORef failedOnChainRef
            when anyFailed $
                error "some blocks failed"

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
