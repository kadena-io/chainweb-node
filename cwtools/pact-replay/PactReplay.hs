{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.PruneForks qualified as PruneForks
import Chainweb.Cut (cutHeaders, unsafeMkCut)
import Chainweb.Cut.Create hiding (join)
import Chainweb.CutDB (cutHashesTable, readHighestCutHeaders)
import Chainweb.Logger
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.PactService qualified as PactService
import Chainweb.Pact.Payload.PayloadStore.RocksDB qualified as Pact.Payload.PayloadStore.RocksDB
import Chainweb.Pact.Types
import Chainweb.PayloadProvider (blockHeaderToEvaluationCtx)
import Chainweb.PayloadProvider.Pact
import Chainweb.PayloadProvider.Pact.Genesis (genesisPayload)
import Chainweb.Storage.Table.RocksDB (modernDefaultOptions, withRocksDb)
import Chainweb.TreeDB qualified as TreeDB
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Registry
import Chainweb.WebBlockHeaderDB
import Control.Concurrent.Async (forConcurrently_, forConcurrently)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Constraint
import Data.HashSet qualified as HS
import Data.Text.IO qualified as T
import GHC.Stack
import Options.Applicative
import Streaming qualified as S
import Streaming.Prelude qualified as S
import System.FilePath ((</>))
import System.LogLevel
import Chainweb.Parent
import qualified Data.Sequence as Seq
import Chainweb.Core.Brief
import Chainweb.BlockHeight (BlockHeight(..))
import Data.IORef

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
    chains :: Dict HasVersion -> [ChainId] <-
        fmap const (jsonOption (long "chains"))
        <|> pure (\Dict -> filter isPactChain (HS.toList chainIds))
    return $ withVersion version $ do
        withRocksDb (getRocksDbDir dbDir) modernDefaultOptions $ \rdb -> do
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
                serviceEnv <- PactService.withPactService
                    cid Nothing mempty logger Nothing pdb readOnlySqlPool readWriteSqlenv config
                    (GenesisPayload $ fromJuste $ genesisPayload cid)

                failedOnChainRef <- liftIO $ newIORef False
                bhdb <- getWebBlockHeaderDb wbhdb cid

                let upperEndBlock = limitedCut ^?! cutHeaders . ix cid
                let upper = (HS.singleton (TreeDB.UpperBound $ view blockHash upperEndBlock))
                liftIO $ TreeDB.branchEntries bhdb Nothing Nothing Nothing Nothing mempty upper $ \blockStream -> do
                    blockStream
                        & S.takeWhile (\blk -> maybe True (\start -> view blockHeight blk >= start) maybeStart)
                        & S.slidingWindow 2
                        & S.chunksOf 200
                        & S.mapped S.toList
                        & S.mapM_ (\blkChunk -> do
                            logFunctionText logger Info $
                                "Replaying chunk from " <> brief (Seq.index (head blkChunk) 0) <> " to " <> brief (Seq.index (last blkChunk) 0)

                            errs <- PactService.execReadOnlyReplay logger serviceEnv $
                                [ view blockPayloadHash h <$ blockHeaderToEvaluationCtx (Parent ph)
                                | (h Seq.:<| ph Seq.:<| Seq.Empty) <- blkChunk
                                ]
                            when (not (null errs)) $
                                writeIORef failedOnChainRef True
                            forM errs $ \(evalCtx, err) -> do
                                logFunctionText logger Error $ "Invalid block with ctx " <> brief evalCtx <> ": " <> sshow err
                            )

                liftIO $ logFunctionText logger Info $ "finished replaying chain " <> brief cid

                liftIO $ readIORef failedOnChainRef
            when anyFailed $
                error "some blocks failed"
