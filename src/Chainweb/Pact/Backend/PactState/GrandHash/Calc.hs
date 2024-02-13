{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Calculate the 'Snapshots'es against a pact database.
--
--   The 'pactCalcMain' tool (included in cwtool as pact-calc) allows
--   users to calculate the snapshots, as well as generate haskell modules
--   that embed the snapshots in them.
module Chainweb.Pact.Backend.PactState.GrandHash.Calc
  (
    pactCalcMain

  , pactCalc
  , BlockHeightTargets(..)
  )
  where

import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.ChainId (ChainId, chainIdToText)
import Chainweb.Logger (Logger, logFunctionText)
import Chainweb.Pact.Backend.Compaction qualified as C
import Chainweb.Pact.Backend.PactState (allChains)
import Chainweb.Pact.Backend.PactState.EmbeddedSnapshot (Snapshot(..))
import Chainweb.Pact.Backend.PactState.GrandHash.Algorithm (ChainGrandHash(..))
import Chainweb.Pact.Backend.PactState.GrandHash.Utils (resolveLatestCutHeaders, resolveCutHeadersAtHeights, computeGrandHashesAt, withConnections, hex, rocksParser, cwvParser)
import Chainweb.Pact.Backend.Types (SQLiteEnv(..))
import Chainweb.Storage.Table.RocksDB (RocksDb, withReadOnlyRocksDb, modernDefaultOptions)
import Chainweb.Version (ChainwebVersion(..), ChainwebVersionName(..))
import Chainweb.Version.Development (devnet)
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.RecapDevelopment (recapDevnet)
import Chainweb.Version.Testnet (testnet)
import Control.Applicative ((<|>), many)
import Control.Monad (when)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC8
import Data.Char qualified as Char
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Options.Applicative (ParserInfo, Parser, (<**>))
import Options.Applicative qualified as O
import Pact.JSON.Encode qualified as J
import System.LogLevel (LogLevel(..))
import UnliftIO.Async (pooledForConcurrently)

-- | Calculate hashes at specified blockheight(s).
data BlockHeightTargets
  = LatestAll
    -- ^ Latest blockheight of consensus.
  | TargetAll (Set BlockHeight)
    -- ^ Target blockheights across all chains. If a blockheight is not
    --   accessible from a chain, it is skipped.

-- | TODO: DOC
pactCalc :: (Logger logger)
  => logger
  -> ChainwebVersion
  -> HashMap ChainId SQLiteEnv
     -- ^ pact database dir
  -> RocksDb
     -- ^ rocksdb dir
  -> BlockHeightTargets
     -- ^ target for calculation
  -> IO [(BlockHeight, HashMap ChainId Snapshot)]
pactCalc logger v pactConns rocksDb targets = do
  logFunctionText logger Debug "Starting pact-calc"

  chainTargets <- case targets of
    LatestAll -> do
      List.singleton <$> resolveLatestCutHeaders logger v pactConns rocksDb
    TargetAll ts -> do
      resolveCutHeadersAtHeights logger v pactConns rocksDb (Set.toDescList ts)

  pooledForConcurrently chainTargets $ \(b, cutHeader) -> do
    fmap (b,) $ computeGrandHashesAt pactConns cutHeader

data PactCalcConfig = PactCalcConfig
  { pactDir :: FilePath
  , rocksDir :: FilePath
  , chainwebVersion :: ChainwebVersion
  , target :: BlockHeightTargets
  , writeModule :: Bool
  }

-- | Calculate the hash at every provided blockheight across all chains.
--
--   Note that for some chains, one or more of the requested blockheights
--   won't be accessible. This could be due to compaction, or the blockheight
--   predating the genesis of the chain. In this case, a warning will be
--   emitted, not an error.
pactCalcMain :: IO ()
pactCalcMain = do
  cfg <- O.execParser opts
  C.withDefaultLogger Debug $ \logger -> do
    withConnections logger cfg.pactDir (allChains cfg.chainwebVersion) $ \pactConns -> do
      withReadOnlyRocksDb cfg.rocksDir modernDefaultOptions $ \rocksDb -> do
        chainHashes <- pactCalc logger cfg.chainwebVersion pactConns rocksDb cfg.target
        when cfg.writeModule $ do
          let modulePath = "src/Chainweb/Pact/Backend/PactState/EmbeddedSnapshot/" <> versionModuleName cfg.chainwebVersion <> ".hs"
          writeFile modulePath (chainHashesToModule cfg.chainwebVersion chainHashes)
        BLC8.putStrLn $ grandsToJson chainHashes
  where
    opts :: ParserInfo PactCalcConfig
    opts = O.info (parser <**> O.helper) (O.fullDesc <> O.progDesc helpText)

    helpText :: String
    helpText = unlines
      [ "Compute the grand hash of a Pact database at a particular height(s)."
      , "If no height is specified, defaults to the latest state of consensus."
      ]

    parser :: Parser PactCalcConfig
    parser = PactCalcConfig
      <$> O.strOption
            (O.long "pact-database-dir"
             <> O.short 'd'
             <> O.metavar "PACT DBDIR"
             <> O.help "Source Pact database, which you wish to verify"
            )
      <*> rocksParser
      <*> cwvParser
      <*> targetsParser
      <*> O.switch (O.long "write-module" <> O.help "Write the hashes and headers out as a Haskell module to be used in chainweb-node")

targetsParser :: Parser BlockHeightTargets
targetsParser =
  let
    makeTargeted = \case
      [] -> LatestAll
      ts -> TargetAll (Set.fromList ts)
    p = fmap makeTargeted $ many $ fmap BlockHeight $ O.option O.auto
          (O.long "target-blockheight"
            <> O.short 'b'
            <> O.metavar "BLOCKHEIGHT"
            <> O.help "BlockHeight to verify."
          )
  in
  p <|> pure LatestAll

grandsToJson :: [(BlockHeight, HashMap ChainId Snapshot)] -> BL.ByteString
grandsToJson chainHashes =
  J.encode $ J.Object $ flip List.map chainHashes $ \(height, hashes) ->
    let sortedHashes = List.sortOn fst $ HM.toList hashes
        key = Text.pack $ show height
        val = J.Object $ flip List.map sortedHashes $ \(cid, Snapshot hash header) ->
                let o = J.Object
                      [ "hash" J..= hex (getChainGrandHash hash)
                      , "header" J..= J.encodeWithAeson header
                      ]
                in (chainIdToText cid, o)
    in (key J..= val)

-- | Output a Haskell module with the embedded hashes. This module is produced
--   by pact-calc, and embedded into the chainweb-node library.
--
--   The implementation is a little janky, but it works.
chainHashesToModule :: ChainwebVersion -> [(BlockHeight, HashMap ChainId Snapshot)] -> String
chainHashesToModule v input = prefix
  where
    indent :: Int -> String -> String
    indent n s = List.replicate n ' ' ++ s

    onHead :: (a -> a) -> [a] -> [a]
    onHead f = \case { [] -> []; x : xs -> f x : xs; }

    onTail :: (a -> a) -> [a] -> [a]
    onTail f = \case { [] -> []; x : xs -> x : List.map f xs; }

    inQuotes :: String -> String
    inQuotes s = "\"" ++ s ++ "\""

    embedQuotes :: String -> String
    embedQuotes = \case
      [] -> []
      c : cs -> "\"\\" ++ [c] ++ go cs
      where
        go = \case
          [] -> error "mis-use of embedQuotes"
          [_] -> error "mis-use of embedQuotes"
          xs -> List.init xs ++ "\\\"" ++ [List.last xs]

    prepend :: String -> (String -> String)
    prepend p = \s -> p ++ s

    formatUnderscores :: (Integral a, Show a) => a -> String
    formatUnderscores n = reverse $ List.intercalate "_" $ chunksOf 3 $ reverse $ show n
      where
        chunksOf k =
          let
            go = \case
              [] -> []
              xs -> take k xs : go (drop k xs)
          in
          go

    makeEntries :: [(BlockHeight, HashMap ChainId Snapshot)] -> [String]
    makeEntries =
      List.concatMap (List.map (indent 4))
      . onTail (onTail (indent 2) . onHead (prepend ", "))
      . List.map (uncurry makeEntry)

    makeEntry :: BlockHeight -> HashMap ChainId Snapshot -> [String]
    makeEntry height chainMap =
      [ "( BlockHeight " ++ formatUnderscores height
      , ", HM.fromList"
      , "    ["
      ]
      ++ onHead ("  " ++) (List.map (indent 4) $ onTail (prepend ", ") (makeChainMap chainMap))
      ++
      [ "    ]"
      , ")"
      ]

    makeChainMap :: HashMap ChainId Snapshot -> [String]
    makeChainMap = map (uncurry makeChainEntry) . List.sortOn fst . HM.toList

    makeChainEntry :: ChainId -> Snapshot -> String
    makeChainEntry cid (Snapshot hash header) =
      let
        jsonDecode j = "unsafeDecodeBlockHeader " ++ j
        fromHex b = "unsafeBase16Decode " ++ b
        sCid = Text.unpack (chainIdToText cid)
        sHash = inQuotes $ Text.unpack (hex (getChainGrandHash hash))
        sHeader = embedQuotes $ Text.unpack (J.encodeText (J.encodeWithAeson header))
      in
      concat
        [ "(unsafeChainId " ++ sCid ++ ", "
        , "Snapshot (ChainGrandHash (" ++ fromHex sHash ++ ")) (" ++ jsonDecode sHeader ++ ")"
        , ")"
        ]

    prefix = List.unlines
      [ "-- NOTE:"
      , "-- This module has been auto-generated by https://github.com/kadena-io/chainweb-node/blob/master/src/Chainweb/Pact/Backend/PactState/GrandHash.hs"
      , "-- Do not edit it."
      , ""
      , "{-# LANGUAGE ImportQualifiedPost #-}"
      , "{-# LANGUAGE NumericUnderscores #-}"
      , "{-# LANGUAGE OverloadedStrings #-}"
      , ""
      , "module Chainweb.Pact.Backend.PactState.EmbeddedSnapshot." <> versionModuleName v
      , "  ( grands"
      , "  )"
      , "  where"
      , ""
      , "import Chainweb.BlockHeight (BlockHeight(..))"
      , "import Chainweb.ChainId (ChainId, unsafeChainId)"
      , "import Chainweb.Pact.Backend.PactState.EmbeddedSnapshot (Snapshot(..), unsafeDecodeBlockHeader, unsafeBase16Decode)"
      , "import Chainweb.Pact.Backend.PactState.GrandHash.Algorithm (ChainGrandHash(..))"
      , "import Data.HashMap.Strict (HashMap)"
      , "import Data.HashMap.Strict qualified as HM"
      , "import Data.List qualified as List"
      , "import Data.Ord (Down(..))"
      , ""
      , "-- | sorted in descending order."
      , "grands :: [(BlockHeight, HashMap ChainId Snapshot)]"
      , "grands = List.sortOn (Down . fst)"
      , "  ["
      , unlines (makeEntries input)
      , "  ]"
      ]

versionModuleName :: ChainwebVersion -> String
versionModuleName v
  | v == mainnet = "Mainnet"
  | v == testnet = "Testnet"
  | v == recapDevnet = "RecapDevnet"
  | v == devnet = "Devnet"
  | otherwise = case Text.unpack (getChainwebVersionName (_versionName v)) of
      [] -> []
      c : cs -> Char.toUpper c : cs
