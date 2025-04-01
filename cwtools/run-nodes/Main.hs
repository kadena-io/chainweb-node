{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Chainweb.Graph (petersenChainGraph)
import Chainweb.Test.TestVersions
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Registry
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Data.Word
import Options.Applicative
import System.Directory (executable, getPermissions)
import System.Process (callProcess)
import Data.Text qualified as T

data Env = Env
  { exe :: FilePath
  , nodes :: Word8
  , version :: ChainwebVersion
  , config :: FilePath
  , passthrough :: [T.Text]
  } deriving (Show)

pEnv :: Parser Env
pEnv = Env <$> pExe <*> pNodes <*> pVersion <*> pConfig <*> many pPassthrough

pExe :: Parser FilePath
pExe = strOption
  (long "exe" <> metavar "PATH"
   <> help "Full path to a chainweb-node binary")

pNodes :: Parser Word8
pNodes = option auto
  (long "nodes" <> metavar "COUNT" <> value 10
   <> help "Number of Nodes to run (default: 10)")

pVersion :: Parser ChainwebVersion
pVersion = option (findKnownVersion =<< textReader)
  (long "chainweb-version" <> metavar "CHAINWEB_VERSION"
   <> value (fastForkingCpmTestVersion petersenChainGraph)
   <> help "Chainweb Version to run the Nodes with (default: timedCPM-petersen)")

pConfig :: Parser FilePath
pConfig = strOption
  (long "config" <> metavar "PATH" <> value "tools/run-nodes/test-bootstrap-node.config"
   <> help "Path to Chainweb config YAML file")

pPassthrough :: Parser T.Text
pPassthrough = argument str
  (metavar "CHAINWEB-FLAGS" <> help "Native flags that a chainweb-node accepts")

runNode :: Word8 -> Maybe FilePath -> Env -> IO ()
runNode nid mconf (Env e ns v _ ps) = callProcess e (T.unpack <$> ops)
  where
    ops :: [T.Text]
    ops = [ "--hostname=127.0.0.1"
          , "--node-id=" <> sshow nid
          , "--test-miners=" <> sshow ns
          , "--chainweb-version=" <> getChainwebVersionName (_versionName v)
          , "--interface=127.0.0.1"
          ]
          <> maybe [] (\c -> ["--config-file=" <> T.pack c]) mconf
          <> ps
          <> [ "+RTS", "-T" ]

main :: IO ()
main = do
  env@(Env e ns _ c _) <- execParser runNodesOpts
  print env
  canExec <- (executable <$> getPermissions e) `catch` (\(_ :: SomeException) -> pure False)
  if | not canExec -> error $ e <> " is not executable, or does not exist."
     | otherwise -> do
         putStrLn "Starting cluster..."
         -- Launch Bootstrap Node
         withAsync (runNode 0 (Just c) env) $ \boot -> do
           link boot
           threadDelay 1000000  -- 1 second
           -- Launch Common Nodes
           mapConcurrently_ (\n -> runNode n Nothing env) [1 .. ns - 1]


runNodesOpts :: ParserInfo Env
runNodesOpts = info (pEnv <**> helper)
    (fullDesc <> progDesc "Run a local cluster of chainweb-node binaries")
