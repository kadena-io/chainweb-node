{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Tools.RunNodes ( main, runNodesOpts ) where

import BasePrelude hiding (option, (%))
import Chainweb.Graph (petersonChainGraph)
import Chainweb.Version
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Error.Util (note)
import qualified Data.Text as T
import Formatting
import Options.Applicative
import Shelly hiding (FilePath)
import System.Directory (executable, getPermissions)

---

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
pVersion = option cver
  (long "chainweb-version" <> metavar "CHAINWEB_VERSION"
   <> value (TimedCPM petersonChainGraph)
   <> help "Chainweb Version to run the Nodes with (default: timedCPM-peterson)")
  where
    cver :: ReadM ChainwebVersion
    cver = eitherReader $ \s ->
        note "Illegal ChainwebVersion" . chainwebVersionFromText $ T.pack s

pConfig :: Parser FilePath
pConfig = strOption
  (long "config" <> metavar "PATH" <> value "tools/run-nodes/test-bootstrap-node.config"
   <> help "Path to Chainweb config YAML file")

pPassthrough :: Parser T.Text
pPassthrough = argument str
  (metavar "CHAINWEB-FLAGS" <> help "Native flags that a chainweb-node accepts")

runNode :: Word8 -> Maybe FilePath -> Env -> IO ()
runNode nid mconf (Env e ns v _ ps) = shelly $ run_ (fromText $ T.pack e) ops
  where
    ops :: [T.Text]
    ops = [ "--hostname=127.0.0.1"
          , sformat ("--node-id=" % int) nid
          , sformat ("--test-miners=" % int) ns
          , sformat ("--chainweb-version=" % stext) $ chainwebVersionToText v
          , "--interface=127.0.0.1" ]
          <> maybe [] (\c -> [sformat ("--config-file=" % string) c]) mconf
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
