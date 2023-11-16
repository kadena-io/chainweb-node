{-# LANGUAGE OverloadedStrings #-}
module Main where

import Chainweb.Version.Development
import Chainweb.Version.FastDevelopment
import Chainweb.Version.Registry

import System.Environment
import System.Exit
import Text.Printf

import Chainweb.Pact.Backend.Compaction (main)
import Chainweb.Pact.Backend.PactState (pactDiffMain)

import qualified CheckpointerDBChecksum
import qualified Ea
import qualified EncodeDecodeB64Util
import qualified GenConf
import qualified HeaderDump
import qualified RunNodes
import qualified SlowTests
import qualified TxStream
import qualified KnownGraphs
import qualified TxSimulator
import qualified CalculateRelease

main :: IO ()
main = do
    registerVersion Development
    registerVersion FastDevelopment
    args <- getArgs
    case args of
      [] -> printHelp topLevelCommands
      ["-h"] -> printHelp topLevelCommands
      ["--help"] -> printHelp topLevelCommands
      (cmd:restOfArgs) -> do
        case filter (\cs -> cmd == csCmd cs) topLevelCommands of
          [] -> do
            printf "Error: \"%s\" is not a valid command\n\n" cmd
            printHelp topLevelCommands
            exitFailure
          [cs] -> do
            progName <- getProgName
            withArgs restOfArgs $ withProgName (unwords [progName, cmd]) $ csAction cs
          _ -> error "Duplicate command encountered.  This shouldn't happen!"

data CommandSpec = CommandSpec
    { csCmd :: String
    , csDescription :: String
    , csAction :: IO ()
    }

padRight :: Int -> String -> String
padRight n s = s ++ padding
  where
    padding = replicate (max 0 (n - length s)) ' '


cmdHelpLine :: CommandSpec -> String
cmdHelpLine cs = printf "  %s%s" (padRight 25 $ csCmd cs) (csDescription cs)

topLevelCommands :: [CommandSpec]
topLevelCommands =
  [ CommandSpec
      "ea"
      "Generate Chainweb genesis blocks and their payloads"
      Ea.main
  , CommandSpec
      "run-nodes"
      "Run a local cluster of chainweb-node binaries"
      RunNodes.main
  , CommandSpec
      "slow-tests"
      "Run slow Chainweb tests"
      SlowTests.main
  , CommandSpec
      "tx-list"
      "List all transactions in a chain starting with the most recent block"
      TxStream.main
  , CommandSpec
      "genconf"
      "Interactively generate a chainweb-node config"
      GenConf.main
  , CommandSpec
      "header-dump"
      "Dump Block Headers to a JSON array"
      HeaderDump.main
  , CommandSpec
      "b64"
      "Command line utlis for Chainweb's base64 encode/decode"
      EncodeDecodeB64Util.main
  , CommandSpec
      "db-checksum"
      "Generate a checksum of all the checkpointer database tables between\
      \\n an inclusive range of blocks."
      CheckpointerDBChecksum.main
  , CommandSpec
      "known-graphs"
      "Encode know graphs as JSON values"
      KnownGraphs.main
  , CommandSpec
      "tx-sim"
      "Simulate tx execution against real pact dbs"
      TxSimulator.simulateMain
  , CommandSpec
      "compact"
      "Compact pact database"
      Chainweb.Pact.Backend.Compaction.main
  , CommandSpec
      "pact-diff"
      "Diff the latest state of two pact databases"
      Chainweb.Pact.Backend.PactState.pactDiffMain
  , CommandSpec
      "calculate-release"
      "Calculate next service date and block heights for upgrades"
      CalculateRelease.main
  ]

printHelp :: [CommandSpec] -> IO ()
printHelp commands = do
    progName <- getProgName
    putStrLn $ unlines $
      header progName ++
      map cmdHelpLine commands
  where
    header progName =
      [ "Chainweb Tool"
      , ""
      , "This executable contains misc commands that have been created for various"
      , "reasons in the course of Chainweb development. Linking executables is slow and"
      , "the resulting binaries are large, so it is more efficient in terms of build"
      , "time, space usage, download time, etc to consolidate them into one binary."
      , ""
      , "Usage: "++progName++" COMMAND"
      , ""
      , "Available options:"
      ,   "  -h,--help                Show this help text"
      , ""
      , "Available commands:"
      ]
