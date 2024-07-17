{-# language
    ImportQualifiedPost
  , OverloadedRecordDot
  , OverloadedStrings
  , ScopedTypeVariables
#-}

module Main (main) where

import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Chainweb (ChainwebStatus(..))
import Chainweb.Chainweb.Configuration (ChainwebConfiguration(..), defaultChainwebConfiguration, defaultCutConfig, configP2p, configReadOnlyReplay, configOnlySyncPact, configCuts, cutInitialBlockHeightLimit, cutFastForwardBlockHeightLimit)
import Chainweb.Logger (logFunctionJson, logFunctionText)
import Chainweb.Utils (fromText)
import Chainweb.Version (ChainwebVersion(..))
import Chainweb.Version.Mainnet (mainnet)
import Chainweb.Version.Registry (lookupVersionByName, registerVersion)
import ChainwebNode (ChainwebNodeConfiguration(..), defaultChainwebNodeConfiguration, nodeConfigDatabaseDirectory, nodeConfigChainweb, node, withNodeLogger, withServiceDate)
import Control.Applicative (optional)
import Control.Exception (SomeException, SomeAsyncException, Handler(..), catches, throwIO)
import Control.Lens ((?~), (.~))
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Options.Applicative qualified as O
import P2P.Node.Configuration (defaultP2pConfiguration, p2pConfigBootstrapReachability, p2pConfigIgnoreBootstrapNodes)
import System.IO qualified as IO
import System.LogLevel (LogLevel(..))

main :: IO ()
main = do
  cfg <- getConfig
  let nodeConfig = mkReplayConfiguration cfg
  registerVersion cfg.chainwebVersion
  IO.hSetBuffering IO.stderr IO.LineBuffering
  withNodeLogger (_nodeConfigLog nodeConfig) (_nodeConfigChainweb nodeConfig) cfg.chainwebVersion $ \logger -> do
    logFunctionJson logger Info ProcessStarted
    flip catches
      [ Handler $ \(e :: SomeAsyncException) ->
          logFunctionJson logger Info (ProcessDied $ show e) >> throwIO e
      , Handler $ \(e :: SomeException) ->
          logFunctionJson logger Error (ProcessDied $ show e) >> throwIO e
      ] $ do
      kt <- mapM iso8601ParseM (_versionServiceDate cfg.chainwebVersion)
      withServiceDate (_configChainwebVersion (_nodeConfigChainweb nodeConfig)) (logFunctionText logger) kt $ node nodeConfig logger

mkReplayConfiguration :: Config -> ChainwebNodeConfiguration
mkReplayConfiguration cfg = defaultChainwebNodeConfiguration
  & nodeConfigDatabaseDirectory ?~ cfg.databaseDir
  & nodeConfigChainweb .~ cwConfig
  where
    cwConfig = defaultChainwebConfiguration mainnet
      & configReadOnlyReplay .~ cfg.readOnly
      & configOnlySyncPact .~ not cfg.readOnly
      & configCuts .~ (defaultCutConfig & cutInitialBlockHeightLimit .~ cfg.initialBlockHeightLimit & cutFastForwardBlockHeightLimit .~ cfg.fastForwardBlockHeightLimit)
      & configP2p .~ (defaultP2pConfiguration & p2pConfigBootstrapReachability .~ 0 & p2pConfigIgnoreBootstrapNodes .~ True)

data Config = Config
  { chainwebVersion :: ChainwebVersion
  , readOnly :: Bool
  , databaseDir :: FilePath
  , initialBlockHeightLimit :: Maybe BlockHeight
  , fastForwardBlockHeightLimit :: Maybe BlockHeight
  }

getConfig :: IO Config
getConfig = do
  O.execParser opts
  where
    opts :: O.ParserInfo Config
    opts = O.info (parser O.<**> O.helper) (O.fullDesc <> O.progDesc "Chainweb Replay tool")

    parser :: O.Parser Config
    parser = Config
      <$> (parseVersion <$> O.strOption (O.long "chainweb-version" <> O.help "chainweb version (e.g. mainnet01, testnet04)" <> O.value "mainnet01"))
      <*> O.switch (O.long "read-only" <> O.help "Run in read-only mode")
      <*> O.strOption (O.long "database-dir" <> O.help "Path to the database directory")
      <*> optional (BlockHeight <$> O.option O.auto (O.long "initial-block-height-limit" <> O.help "Initial block height limit"))
      <*> optional (BlockHeight <$> O.option O.auto (O.long "fast-forward-block-height-limit" <> O.help "Fast forward block height limit"))

    parseVersion :: Text -> ChainwebVersion
    parseVersion =
      lookupVersionByName
      . fromMaybe (error "ChainwebVersion parse failed")
      . fromText
