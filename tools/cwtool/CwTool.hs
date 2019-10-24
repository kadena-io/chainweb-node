{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import System.Exit
import Text.Printf

import qualified Chain2Gexf
import qualified Ea
import qualified RunNodes
import qualified SlowTests
import qualified TXG
import qualified Standalone
import qualified TestMiner
import qualified TxStream

main :: IO ()
main = do
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
      "chain2gexf"
      "Convert a persisted Chainweb into .gexf format for viewing"
      Chain2Gexf.main
  , CommandSpec
      "txg"
      "Generate a random stream of simulated blockchain transactions"
      TXG.main
  , CommandSpec
      "run-nodes"
      "Run a local cluster of chainweb-node binaries"
      RunNodes.main
  , CommandSpec
      "slow-tests"
      "Run slow Chainweb tests"
      SlowTests.main
  , CommandSpec
      "standalone"
      "Run a leaner chainweb instance for optimization work"
      Standalone.main
  , CommandSpec
      "tx-list"
      "List all transactions in a chain starting with the most recent block"
      TxStream.main
  , CommandSpec
      "test-miner"
      "Test an external miner"
      TestMiner.main
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
