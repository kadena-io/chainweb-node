{-# LANGUAGE OverloadedStrings #-}
module Main where

import Configuration.Utils.CommandLine
import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen (fillSep, text, vcat)

import qualified Chain2Gexf
import qualified Ea
import qualified RunNodes
import qualified SlowTests
import qualified TransactionGenerator

data Command
  = C2Gexf
  | Ea
  | GenTransactions
  | RunNodes
  | SlowTests

commandParser :: Parser Command
commandParser = subparser $
    command "bigbang" eaOpts <> -- Using "ea" as a command seemed to have problems
    command "chain2gexf" (C2Gexf <$ Chain2Gexf.opts) <>
    command "gen-transactions" (GenTransactions <$ tgenOpts) <>
    command "run-nodes" (RunNodes <$ RunNodes.runNodesOpts) <>
    command "slow-tests" (SlowTests <$ slowTestOpts)

tgenOpts :: ParserInfo Command
tgenOpts = info (pure GenTransactions)
    (fullDesc
     <> progDesc "Generate a random stream of simulated blockchain transactions")

eaOpts :: ParserInfo Command
eaOpts = info (pure Ea)
    (fullDesc
     <> progDesc "Generate Chainweb genesis blocks and their payloads")

slowTestOpts :: ParserInfo Command
slowTestOpts = info (pure SlowTests)
    (fullDesc
     <> progDesc "Run slow Chainweb tests")

main :: IO ()
main = do
    cmd <- customExecParser p opts
    case cmd of
      C2Gexf -> Chain2Gexf.main
      Ea -> Ea.main
      GenTransactions -> TransactionGenerator.main
      RunNodes -> RunNodes.main
      SlowTests -> SlowTests.main
  where
    opts = info (commandParser <**> helper) mods
    mods = headerDoc (Just theHeader)
        <> footerDoc (Just theFooter)
    theHeader = vcat
      [ "Chainweb Tool"
      , ""
      , fillSep [ text w | w <- words theDesc ]
      ]
    theDesc = "This executable contains misc commands that have been created for various reasons in the course of Chainweb development.  Linking executables is slow and the resulting binaries are large, so it is more efficient in terms of build time, space usage, download time, etc to consolidate them into one binary."
    theFooter = vcat
      [ "Run the following command to enable tab completion:"
      , ""
      , "source <(cwtool --bash-completion-script `which cwtool`)"
      ]
    p = prefs showHelpOnEmpty
