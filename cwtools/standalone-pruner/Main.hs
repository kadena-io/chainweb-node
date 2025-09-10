{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ApplicativeDo #-}

module Main where

import Options.Applicative

import Control.Monad
import Data.Text.IO qualified as T
import System.LogLevel

import Chainweb.BlockHeaderDB.PruneForks qualified as PruneForks
import Chainweb.Cut (unsafeMkCut)
import Chainweb.CutDB (readHighestCutHeaders, cutHashesTable)
import Chainweb.Logger
import Chainweb.Storage.Table.RocksDB (withRocksDb, modernDefaultOptions)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Registry
import Chainweb.WebBlockHeaderDB

main :: IO ()
main = join $
    execParser $ info
        (parser <**> helper)
        (fullDesc
            <> progDesc "Prune a chainweb-node block header database (rocksdb)"
            <> header "standalone-pruner")

parser :: Parser (IO ())
parser = do
    version <- option (findKnownVersion =<< textReader) (long "chainweb-version" <> short 'v')
    dbDir <- textOption (long "rocksdb-path")
    logLevel <- flag' Debug (long "verbose") <|> flag' Warn (long "quiet") <|> pure Info
    doPrune <-
        flag' PruneForks.ForcePrune (long "force")
        <|> flag' PruneForks.PruneDryRun (long "dry-run")
        <|> pure PruneForks.Prune
    return $ withVersion version $ do
        withRocksDb dbDir modernDefaultOptions $ \rdb -> do
            let logger = genericLogger logLevel T.putStrLn
            wbhdb <- initWebBlockHeaderDb rdb
            let cutHashesStore = cutHashesTable rdb
            initialCut <- unsafeMkCut <$> readHighestCutHeaders (logFunctionText logger) wbhdb cutHashesStore
            PruneForks.pruneForks logger initialCut wbhdb doPrune (int PruneForks.safeDepth)
            return ()
