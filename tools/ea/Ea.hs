{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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

import Ea.Genesis

import System.LogLevel (LogLevel(..))

-- internal modules

import Chainweb.BlockHeaderDB
import Chainweb.Logger (genericLogger)
import Chainweb.Miner.Pact (noMiner)
import Chainweb.Pact.PactService
import Chainweb.Payload.PayloadStore.InMemory
import Chainweb.Time
import Chainweb.Transaction (mkPayloadWithText)
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion(..), someChainId)

import Pact.ApiReq (mkApiReq)
import Pact.Parse
import Pact.Types.ChainMeta
import Pact.Types.Command hiding (Payload)

---

main :: IO ()
main = devnet
    >> fastnet
    >> testnet
    >> mainnet
    >> putStrLn "Done."
  where
    devnet  = mkPayloads [development0, developmentN]
    fastnet = mkPayloads [fastTimedCPM0, fastTimedCPMN]
    testnet = mkPayloads [testnet0, testnetN]
    mainnet = mkPayloads
      [ mainnet0
      , mainnet1
      , mainnet2
      , mainnet3
      , mainnet4
      , mainnet5
      , mainnet6
      , mainnet7
      , mainnet8
      , mainnet9
      ]

-- | Generate paylaods for a traversable of txs
--
mkPayloads :: Traversable t => t Genesis -> IO ()
mkPayloads = traverse_ mkPayload

-- | Generate a payload for a given genesis transaction
--
mkPayload :: Genesis -> IO ()
mkPayload (Genesis v tag cid c k a ns) = do
    printf ("Generating Genesis Payload for %s on " <> show_ cid <> "...\n") $ show v
    genPayloadModule v (tag <> sshow cid) txs
  where
    show_ = \case
      N -> "all chains"
      n -> "Chain " <> show n
    -- coin contract genesis txs
    cc = [fungibleAsset, coinContract, gasPayer]
    -- final tx list.
    -- NB: this is position-sensitive data.
    txs = cc <> toList ns <> toList k <> toList a <> toList c

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
        payloadWO <- withSqliteDb v cid logger Nothing Nothing False $ \env ->
            initPactService' v cid logger bhdb pdb env 1000 $
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
