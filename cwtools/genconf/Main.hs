{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Chainweb.Chainweb.Configuration
import Chainweb.HostAddress
import Chainweb.Miner.Config
import Chainweb.Version.Mainnet
import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Yaml qualified as Y
import P2P.Node.Configuration
import P2P.Peer
import System.Directory
import System.Exit
import System.IO
import System.Process

-- three items to ask the user
-- 1) ask for their hostname (or public ip address)
-- 2) ask for a usable port number (test to see if this is actually open)
-- 3) ask if they want mining coordination turned (the default for this value is yes)

getUserInput ::
     Show b
  => String
  -> Maybe b
  -> (Text -> IO (Maybe b))
  -> Maybe (b -> Either String b)
  -> IO b
getUserInput prompt defaultVal parse safetyCheck = do
    putStr (prompt <> " ")
    hFlush stdout
    input' <- T.getLine
    if T.null input' && isJust defaultVal
      then return $ fromJust defaultVal
      else do
       p <- parse input'
       case p of
         Nothing -> do
           putStrLn "Invalid Input, try again"
           hFlush stdout
           getUserInput prompt defaultVal parse safetyCheck
         Just y -> case safetyCheck of
           Nothing -> return y
           Just f -> case f y of
             Left err -> do
               putStrLn err
               hFlush stdout
               getUserInput prompt defaultVal parse safetyCheck
             Right y' -> return y'

getConf :: IO ChainwebConfiguration
getConf = do
    ip <- getIP
    hostname <- hostnameFromText ip
    host <- getUserInput (hostMsg ip) (Just hostname) (const $ return Nothing) Nothing
    port <- getUserInput portMsg (Just 443) (return . portFromText) Nothing
    coord <- getUserInput mineCoordMsg (Just True) (return . yesorno2Bool) Nothing
    return $
      defaultChainwebConfiguration Mainnet01
      & configMining . miningCoordination . coordinationEnabled .~ coord
      & configP2p . p2pConfigPeer . peerConfigAddr .~ HostAddress host port
  where
    hostMsg ip = "What is your publicly reachable domain name / IP address (default: " <> T.unpack ip <> ")?"
    portMsg = "Which port would you like to use (default: 443)?"
    mineCoordMsg = "Would you like to turn mining coordination (default: yes)?"

main :: IO ()
main = do
    conf <- getConf
    exist <- doesFileExist defaultfile
    if exist
    then do
      getUserInput msg (Just True) (return . yesorno2Bool) Nothing >>= \case
        True -> writeStuff conf
        False -> putStrLn "Not writing configuration file"
      else do
        writeStuff conf
    exitSuccess
  where
    msg = "Would you like to write the configuration to " <> defaultfile <> "?"
    defaultfile = "mainnet.yaml"
    writeStuff c = do
        Y.encodeFile defaultfile c
        putStrLn ("Writing (possibly overwriting) configuration to file " <> defaultfile)

yesorno2Bool :: Text -> Maybe Bool
yesorno2Bool text =
  case T.toLower text of
    "yes" -> Just True
    "no" -> Just False
    _ -> Nothing

getIP :: IO Text
getIP = T.pack . (read @String) <$> readProcess "dig" (words "TXT +short o-o.myaddr.l.google.com @ns1.google.com") ""
