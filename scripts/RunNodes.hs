{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main ( main ) where

import BasePrelude hiding (option, (%))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import Formatting
import Options.Applicative
import Shelly hiding (FilePath)
import System.Directory (doesFileExist)

---

data Env = Env
  { exe :: FilePath
  , nodes :: Word8
  , logTarget :: Maybe Log
  , logLevel :: T.Text
  } deriving (Show)

data Log = Path T.Text | ES T.Text Word deriving (Show)

pEnv :: Parser Env
pEnv = Env <$> pExe <*> pNodes <*> optional pLogTarget <*> pLogLevel

pExe :: Parser FilePath
pExe = strOption
  (long "exe" <> metavar "FILE"
   <> help "Full path to a chainweb-node binary")

pNodes :: Parser Word8
pNodes = option auto
  (long "nodes" <> metavar "COUNT" <> value 10
   <> help "Number of Nodes to run (default: 10)")

pLogTarget :: Parser Log
pLogTarget = option logPath
  (long "log-target" <> metavar "local:PATH|es:ELASTICSEARCH_HOST:PORT"
   <> help "Location to log to (default: STDOUT)")
  where
    logPath :: ReadM Log
    logPath = eitherReader (A.parseOnly (esPath <|> plainPath) . T.pack)

    esPath :: A.Parser Log
    esPath = ES <$> host <*> port
      where
        host = A.string "es:" *> A.takeWhile1 (/= ':') <* A.char ':'
        port = A.decimal

    plainPath :: A.Parser Log
    plainPath = Path <$> (A.string "local:" *> A.takeText)

pLogLevel :: Parser T.Text
pLogLevel = strOption
  (long "log-level" <> metavar "info|warn|error|debug" <> value "info"
   <> help "default: info")

runNode :: Word8 -> Maybe T.Text -> Env -> IO ()
runNode nid config (Env e n lt ll) = shelly $ run_ (fromText $ T.pack e) ops
  where
    logging :: Log -> [T.Text]
    logging l@(ES _ _) =
      [ sformat ("--telemetry-log-handle=" % stext) $ logText l
      , sformat ("--log-handle=" % stext) $ logText l
      ]
    logging l@(Path _) =
      [ sformat ("--telemetry-log-handle=" % stext % "/telemetry.node" % int % ".log") (logText l) nid
      , sformat ("--log-handle=" % stext % "/node" % int % ".log") (logText l) nid
      ]

    logText :: Log -> T.Text
    logText (ES h p) = sformat ("es:" % stext % ":" % int) h p
    logText (Path p) = sformat ("file:" % stext) p

    ops :: [T.Text]
    ops = [ "--hostname=127.0.0.1"
          , sformat ("--node-id=" % int) nid
          , sformat ("--test-miners=" % int) n
          , "--chainweb-version=testWithTime"
          , "--interface=127.0.0.1"
          , sformat ("--log-level=" % stext) ll ]
          <> maybe [] logging lt
          <> maybe [] (\c -> [sformat ("--config-file=" % stext) c]) config
          <> [ "+RTS", "-T" ]

-- | Prep a local log dir for output.
logDir :: Log -> IO ()
logDir (ES _ _) = pure ()
logDir (Path p) = shelly $ mkdir_p (fromText p)

main :: IO ()
main = do
  env@(Env e ns lt _) <- execParser opts
  print env
  traverse_ logDir lt
  whenM (doesFileExist e) $ do
    -- Launch Bootstrap Node
    boot <- async $ runNode 0 (Just "scripts/test-bootstrap-node.config") env
    threadDelay 200000  -- 0.2 seconds
    -- Launch Common Nodes
    mapConcurrently_ (\n -> runNode n Nothing env) [1 .. ns - 1]
    void $ wait boot
  where
    opts = info (pEnv <**> helper)
        (fullDesc <> header "run-nodes - Run a local cluster of chainweb-node binaries")
