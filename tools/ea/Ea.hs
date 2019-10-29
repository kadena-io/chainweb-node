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
-- > of the World; and it was called Eä.  -- The Silmarillion - Valaquenta
--
-- Eä means "to be" in Quenya, the ancient language of Tolkien's elves.
--
module Ea ( main ) where

import BasePrelude

import Control.Lens (set)

import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.CAS.RocksDB
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml

import System.LogLevel (LogLevel(..))

-- internal modules

import Allocations

import Chainweb.BlockHeaderDB
import Chainweb.Graph
import Chainweb.Logger (genericLogger)
import Chainweb.Miner.Pact (noMiner)
import Chainweb.Pact.PactService
import Chainweb.Payload.PayloadStore.InMemory
import Chainweb.Time
import Chainweb.Transaction (mkPayloadWithText)
import Chainweb.Version (ChainwebVersion(..), someChainId)

import Pact.ApiReq (mkApiReq)
import Pact.Parse
import Pact.Types.ChainMeta
import Pact.Types.Command hiding (Payload)
import Pact.Types.SPV (noSPVSupport)

---

main :: IO ()
main = do

    -- Convert csv mainnet data to pact request yamls

    generateAllocations

    -- test payloads on chain 0 and N

    go0 chain0
    goN chainN

    -- mainnet payloads on chains 0 through 10

    goM "0" $ mainnetN mainAllocations0
    goM "1" $ mainnetN mainAllocations1
    goM "2" $ mainnetN mainAllocations2
    goM "3" $ mainnetN mainAllocations3
    goM "4" $ mainnetN mainAllocations4
    goM "5" $ mainnetN mainAllocations5
    goM "6" $ mainnetN mainAllocations6
    goM "7" $ mainnetN mainAllocations7
    goM "8" $ mainnetN mainAllocations8
    goM "9" $ mainnetN mainAllocations9

    putStrLn "Done."
  where
    cc = [fungibleAsset, coinContract, gasPayer]
    devDefaults = cc <> [devNs]
    prodDefaults = cc <> [prodNs]
    mainnetDefaults = cc <> [prodNs, testAllocations]

    chain0 =
      [ (Development, "Development", devDefaults <> [devAllocations, dev0Grants])
      , (FastTimedCPM petersonChainGraph, "FastTimedCPM", devDefaults <> [devAllocations, dev0Grants])
      , (Testnet02, "Testnet", prodDefaults <> [prodAllocations, prod0Grants])
      ]

    chainN =
      [ (Development, "Development",  devDefaults <> [devNGrants])
      , (FastTimedCPM petersonChainGraph, "FastTimedCPM", devDefaults <> [devNGrants])
      , (Testnet02, "Testnet", prodDefaults <> [prodNGrants])
      ]

    mainnetN a =
      [ (Mainnet01, "Mainnet", mainnetDefaults <> [a])
      ]

    go0 cs = for_ cs $ \(v, tag, txs) -> do
        printf ("Generating Genesis Payload for %s on Chain 0...\n") $ show v
        genPayloadModule v (tag <> "0") txs

    goM cid cs = for_ cs $ \(v, tag, txs) -> do
        printf ("Generate Mainnet Genesis Payload for %s on Chain " <> T.unpack cid <> "...\n") $ show v
        genPayloadModule v (tag <> cid) txs

    goN cs = for_ cs $ \(v, tag, txs) -> do
        printf ("Generating Genesis Payload for %s on all other chains...\n") $ show v
        genPayloadModule v (tag <> "N") txs



coinContract :: FilePath
coinContract = "pact/coin-contract/load-coin-contract.yaml"

fungibleAsset :: FilePath
fungibleAsset = "pact/coin-contract/load-fungible-asset.yaml"

gasPayer :: FilePath
gasPayer = "pact/gas-payer/load-gas-payer.yaml"

dev0Grants :: FilePath
dev0Grants = "pact/genesis/testnet/grants0.yaml"

devNGrants :: FilePath
devNGrants = "pact/genesis/testnet/grantsN.yaml"

prod0Grants :: FilePath
prod0Grants = "pact/genesis/prodnet/grants0.yaml"

prodNGrants :: FilePath
prodNGrants = "pact/genesis/prodnet/grantsN.yaml"

devNs :: FilePath
devNs = "pact/genesis/testnet/ns.yaml"

prodNs :: FilePath
prodNs = "pact/genesis/prodnet/ns.yaml"

devAllocations :: FilePath
devAllocations = "pact/genesis/testnet/allocations.yaml"

prodAllocations :: FilePath
prodAllocations = "pact/genesis/prodnet/allocations.yaml"

mainAllocations0 :: FilePath
mainAllocations0 = "pact/genesis/mainnet/mainnet_allocations0.yaml"

mainAllocations1 :: FilePath
mainAllocations1 = "pact/genesis/mainnet/mainnet_allocations1.yaml"

mainAllocations2 :: FilePath
mainAllocations2 = "pact/genesis/mainnet/mainnet_allocations2.yaml"

mainAllocations3 :: FilePath
mainAllocations3 = "pact/genesis/mainnet/mainnet_allocations3.yaml"

mainAllocations4 :: FilePath
mainAllocations4 = "pact/genesis/mainnet/mainnet_allocations4.yaml"

mainAllocations5 :: FilePath
mainAllocations5 = "pact/genesis/mainnet/mainnet_allocations5.yaml"

mainAllocations6 :: FilePath
mainAllocations6 = "pact/genesis/mainnet/mainnet_allocations6.yaml"

mainAllocations7 :: FilePath
mainAllocations7 = "pact/genesis/mainnet/mainnet_allocations7.yaml"

mainAllocations8 :: FilePath
mainAllocations8 = "pact/genesis/mainnet/mainnet_allocations8.yaml"

mainAllocations9 :: FilePath
mainAllocations9 = "pact/genesis/mainnet/mainnet_allocations9.yaml"

testAllocations :: FilePath
testAllocations = "pact/genesis/mainnet/test-allocations.yaml"

---------------------
-- Payload Generation
---------------------

genPayloadModule :: ChainwebVersion -> Text -> [FilePath] -> IO ()
genPayloadModule v tag txFiles =
    withTempRocksDb "chainweb-ea" $ \rocks ->
    withBlockHeaderDb rocks v cid $ \bhdb -> do
        rawTxs <- traverse mkTx txFiles
        cwTxs <- forM rawTxs $ \(_, cmd) -> do
            let cmdBS = fmap TE.encodeUtf8 cmd
                procCmd = verifyCommand cmdBS
            case procCmd of
                f@ProcFail{} -> fail (show f)
                ProcSucc c -> do
                  let t = toTxCreationTime (Time (TimeSpan 0))
                  return $! mkPayloadWithText <$> (c & setTxTime t & setTTL (TTLSeconds $ 2 * 24 * 60 * 60))

        let logger = genericLogger Warn TIO.putStrLn
        pdb <- newPayloadDb
        payloadWO <- initPactService' v cid logger noSPVSupport
                         bhdb pdb Nothing Nothing False $
                         execNewGenesisBlock noMiner (V.fromList cwTxs)

        let payloadYaml = TE.decodeUtf8 $ Yaml.encode payloadWO
            modl = T.unlines $ startModule tag <> [payloadYaml] <> endModule
            fileName = "src/Chainweb/BlockHeader/Genesis/" <> tag <> "Payload.hs"

        TIO.writeFile (T.unpack fileName) modl
  where
    setTxTime = set (cmdPayload . pMeta . pmCreationTime)
    setTTL = set (cmdPayload . pMeta . pmTTL)
    toTxCreationTime :: Time Integer -> TxCreationTime
    toTxCreationTime (Time timespan) = case timeSpanToSeconds timespan of
      Seconds s -> TxCreationTime $ ParsedInteger s
    cid = someChainId v

startModule :: Text -> [Text]
startModule tag =
    [ "{-# LANGUAGE QuasiQuotes #-}"
    , ""
    , "-- This module is auto-generated. DO NOT EDIT IT MANUALLY."
    , ""
    , "module Chainweb.BlockHeader.Genesis." <> tag <> "Payload ( payloadBlock ) where"
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
