{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Ea
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- Generate legal genesis blocks, as well as their payloads.
--
-- > In the beginning Eru, the One, who in the Elvish tongue is named Ilúvatar,
-- > made the Ainur of his thought; and they made a great Music before him. In
-- > this Music the World was begun; for Ilúvatar made visible the song of the
-- > Ainur, and they beheld it as a light in the darkness. And many among them
-- > became enamoured of its beauty, and of its history which they saw beginning
-- > and unfolding as in a vision. Therefore Ilúvatar gave to their vision Being,
-- > and set it amid the Void, and the Secret Fire was sent to burn at the heart
-- > of the World; and it was called Eä.  -- The Silmarillion - Ainulindalë
--
-- Eä means "to be" in Quenya, the ancient language of Tolkien's elves.
--
module Ea ( main ) where

import BasePrelude

import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml

import Options.Applicative

import System.LogLevel (LogLevel(..))

-- internal modules

import Chainweb.Logger (genericLogger)
import Chainweb.Pact.PactService
import Chainweb.Pact.Types
import Chainweb.Transaction (PayloadWithText(..))
import Chainweb.Version

import Pact.ApiReq (mkApiReq)
import Pact.Types.Command hiding (Payload)
import Pact.Types.SPV (noSPVSupport)

---

data Env = Env [FilePath]

pEnv :: Parser Env
pEnv = Env <$> many pTrans

pTrans :: Parser FilePath
pTrans = strOption
    (long "transaction" <> metavar "PATH"
    <> help "Path to YAML file containing a Pact transaction. Can be used multiple times. By default, loads the Coin Contract and Grants files.")

main :: IO ()
main = do
    Env txs0 <- execParser opts
    for_ [Testnet00, Testnet01] $ \v -> do
        let txs = bool txs0 [defCoinContractSig, defCoinContract, defGrants] $ null txs0
        putStrLn $ "Generating Genesis Payload for " <> show v <> "..."
        genPayloadModule v txs
    putStrLn "Done."
  where
    opts = info (pEnv <**> helper)
        (fullDesc <> header "ea - Generate Pact Payload modules")

defCoinContractSig :: FilePath
defCoinContractSig = "pact/coin-contract/load-coin-contract-sig.yaml"

defCoinContract :: FilePath
defCoinContract = "pact/coin-contract/load-coin-contract.yaml"

defGrants :: FilePath
defGrants = "pact/genesis/testnet00/grants.yaml"

moduleName :: ChainwebVersion -> Text
moduleName = T.toTitle . chainwebVersionToText

---------------------
-- Payload Generation
---------------------

genPayloadModule :: ChainwebVersion -> [FilePath] -> IO ()
genPayloadModule v txFiles = do
    rawTxs <- traverse mkTx txFiles
    cwTxs <- forM rawTxs $ \(_, cmd) -> do
        let cmdBS = fmap TE.encodeUtf8 cmd
            procCmd = verifyCommand cmdBS
        case procCmd of
            f@ProcFail{} -> fail (show f)
            ProcSucc c -> return $ fmap (\bs -> PayloadWithText bs (_cmdPayload c)) cmdBS

    let logger = genericLogger Warn TIO.putStrLn

    payloadWO <- initPactService' Nothing (someChainId Testnet00) logger (const noSPVSupport) $
        execNewGenesisBlock noMiner (V.fromList cwTxs)

    let payloadYaml = TE.decodeUtf8 $ Yaml.encode payloadWO
        modl = T.unlines $ startModule v <> [payloadYaml] <> endModule
        fileName = "src/Chainweb/BlockHeader/Genesis/" <> moduleName v <> "Payload.hs"

    TIO.writeFile (T.unpack fileName) modl


startModule :: ChainwebVersion -> [Text]
startModule v =
    [ "{-# LANGUAGE QuasiQuotes #-}"
    , ""
    , "-- This module is auto-generated. DO NOT EDIT IT MANUALLY."
    , ""
    , "module Chainweb.BlockHeader.Genesis." <> moduleName v <> "Payload ( payloadBlock ) where"
    , ""
    , "import Data.Text.Encoding (encodeUtf8)"
    , "import Data.Yaml (decodeThrow)"
    , ""
    , "import NeatInterpolation (text)"
    , ""
    , "import Chainweb.Payload (PayloadWithOutputs)"
    , "import Chainweb.Utils (fromJuste)"
    , ""
    , "payloadBlock :: PayloadWithOutputs"
    , "payloadBlock = fromJuste $ decodeThrow $ encodeUtf8 [text|"
    ]

endModule :: [Text]
endModule =
    [ "|]"
    ]

mkTx :: FilePath -> IO (ByteString,Command Text)
mkTx yamlFile = do
    (_,cmd) <- mkApiReq yamlFile
    pure (encodeJSON cmd,cmd)

encodeJSON :: ToJSON a => a -> ByteString
encodeJSON = BL.toStrict . encodePretty' (defConfig { confCompare = compare })
