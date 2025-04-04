{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Ea
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
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
module Main (main) where

import Chainweb.BlockHeaderDB
import Chainweb.Logger (genericLogger)
import Chainweb.Miner.Pact (noMiner)
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.PactService
import Chainweb.Pact.Types (testPactServiceConfig)
import Chainweb.Pact.Utils (toTxCreationTime, emptyPayload)
import Chainweb.Pact.Transaction qualified as Pact
import Chainweb.Pact.Validations (defaultMaxTTLSeconds)
import Chainweb.Payload
import Chainweb.Payload.PayloadStore.InMemory
import Chainweb.Storage.Table.RocksDB
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Development (pattern Development)
import Chainweb.Version.RecapDevelopment (pattern RecapDevelopment)
import Chainweb.Version.Registry (registerVersion)
import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Data.Aeson qualified as Aeson
import Data.Foldable
import Data.Functor
import Data.Pool qualified as Pool
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Traversable
import Data.Vector qualified as V
import Ea.Genesis
import GHC.Exts(the)
import System.LogLevel
import System.IO.Temp
import Text.Printf
import Pact.Core.Command.Types
import Pact.Core.Command.Client
import Pact.Core.ChainData
import qualified Pact.Core.Command.Types as Pact
import qualified Chainweb.ChainId as Chainweb
import qualified Pact.JSON.Encode as J
import Pact.Core.StableEncoding

---

main :: IO ()
main = do
    registerVersion RecapDevelopment
    registerVersion Development

    mapConcurrently_ id
      [ devnet
      -- , fastnet
      , instantnet
      -- , pact5Instantnet
      , quirkedPact5Instantnet
      , testnet05
      -- , genCoinV6Payloads
      ]
    putStrLn "Done."
  where
    devnet = mkPayloads
      [ fastDevelopment0
      , fastDevelopmentN
      ]
    -- fastnet = mkPayloads [fastTimedCPM0, fastTimedCPMN]
    instantnet = mkPayloads [instantCPM0, instantCPMN]
    -- pact5Instantnet = mkPayloads [pact5InstantCPM0, pact5InstantCPMN]
    quirkedPact5Instantnet = mkPayloads [quirkedPact5InstantCPM0, quirkedPact5InstantCPMN]
    testnet04 = mkPayloads [testnet040, testnet04N]
    testnet05 = mkPayloads [testnet050, testnet05N]
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
      , mainnetKAD
      ]

show_ :: ChainIdRange -> String
show_ (ChainIdRange n n')
    | n == n' = "Chain " <> show n
    | otherwise = "Chains " <> show n <> "-" <> show n'

fullGenesisTag :: Genesis -> Text
fullGenesisTag (Genesis _ tag cidr _ _ _ _ _) = tag <> T.pack (chainIdRangeTag cidr)

-- | Generate payloads for a traversable of txs
--
mkPayloads :: Traversable t => t Genesis -> IO ()
mkPayloads = traverse_ (\g -> writePayload g =<< mkPayload g)

writePayload :: Genesis -> Text -> IO ()
writePayload gen payload = do
    let fileName = "src/Chainweb/BlockHeader/Genesis/" <> fullGenesisTag gen <> "Payload.hs"
    TIO.writeFile (T.unpack fileName) payload

-- | Generate a payload for a given list of genesis transactions
--
mkPayload :: Genesis -> IO Text
mkPayload gen@(Genesis v _ cidr@(ChainIdRange l u) c k a ns cc) = do
    -- printf ("Generating Genesis Payload for %s on " <> show_ cidr <> "...\n") $ show v
    payloadModules <- for [l..u] $ \cid ->
        genPayloadModule v (fullGenesisTag gen) (unsafeChainId cid) =<< mkChainwebTxs txs
    -- checks that the modules on each chain are the same
    evaluate $ the payloadModules
  where
    -- final tx list.
    -- NB: this is position-sensitive data.
    txs :: [FilePath]
    txs = cc <> toList ns <> toList k <> toList a <> toList c

---------------------
-- Payload Generation
---------------------

genPayloadModule :: ChainwebVersion -> Text -> Chainweb.ChainId -> [Pact.Transaction] -> IO Text
genPayloadModule v tag cid cwTxs = do
    let logger = genericLogger Warn TIO.putStrLn
    pdb <- newPayloadDb
    withSystemTempDirectory "ea-pact-db" $ \pactDbDir -> do
        payloadWO <- withSqliteDb cid logger pactDbDir False $ \readWriteSql -> do
            roPool <- Pool.newPool $ Pool.defaultPoolConfig (startReadSqliteDb cid logger pactDbDir) stopSqliteDb 10 10
            withPactService v cid Nothing mempty logger Nothing pdb roPool readWriteSql (testPactServiceConfig emptyPayload) $ \serviceEnv ->
                execNewGenesisBlock logger serviceEnv (V.fromList cwTxs)
        return $ TL.toStrict $ TB.toLazyText $ payloadModuleCode tag payloadWO

mkChainwebTxs :: [FilePath] -> IO [Pact.Transaction]
mkChainwebTxs txFiles = mkChainwebTxs' =<< traverse mkTx txFiles

mkChainwebTxs' :: [Command Text] -> IO [Pact.Transaction]
mkChainwebTxs' rawTxs =
    forM rawTxs $ \cmd -> do
        let parsedTx = either (error . sshow) (cmdPayload . pMeta %~ _stableEncoding) $ Pact.unsafeParseCommand (T.encodeUtf8 <$> cmd)
        -- TODO: why is this always the tx creation time? different versions
        -- have different block creation times
        let epochCreationTime = toTxCreationTime (Time (TimeSpan 0))
        let tx' = parsedTx
              & set (cmdPayload . pMeta . pmCreationTime) epochCreationTime
              & set (cmdPayload . pMeta . pmTTL) (TTLSeconds defaultMaxTTLSeconds)
        return $! Pact.unsafeMkPayloadWithText
          (tx' ^. cmdPayload)
          (J.encodeStrict $ (tx' ^. cmdPayload) & pMeta %~ StableEncoding & fmap _pcCode) <$ parsedTx

mkTx :: FilePath -> IO (Command Text)
mkTx yamlFile = snd <$> mkApiReq yamlFile

-- -------------------------------------------------------------------------- --
-- Payload Module

payloadModuleCode :: Text -> PayloadWithOutputs -> TB.Builder
payloadModuleCode t p = foldMap (<> "\n")
    [ "{-# LANGUAGE OverloadedStrings #-}"
    , ""
    , "-- This module is auto-generated. DO NOT EDIT IT MANUALLY."
    , ""
    , "module Chainweb.BlockHeader.Genesis." <> tag <> "Payload ( payloadBlock ) where"
    , ""
    , "import qualified Data.Text as T"
    , "import qualified Data.Vector as V"
    , "import GHC.Stack"
    , ""
    , "import Chainweb.Payload"
    , "import Chainweb.Utils (unsafeFromText, toText)"
    , ""
    , "payloadBlock :: HasCallStack => PayloadWithOutputs"
    , "payloadBlock"
    , "    | actualHash == expectedHash = payload"
    , "    | otherwise = error"
    , "        $ \"inconsistent genesis payload detected. THIS IS A BUG in chainweb-node\""
    , "        <> \". Expected: \" <> T.unpack (toText expectedHash)"
    , "        <> \", actual: \" <> T.unpack (toText actualHash)"
    , "  where"
    , "    actualHash, expectedHash :: BlockPayloadHash"
    , "    actualHash = _payloadWithOutputsPayloadHash payload"
    , "    expectedHash = " <> hash
    , ""
    , "    payload = newPayloadWithOutputs minerData coinbase txs"
    , "    minerData = " <> minerData
    , "    coinbase = " <> coinbase
    , "    txs = V.fromList"
    , "        [ " <> sep "\n        , " tuple (_payloadWithOutputsTransactions p)
    , "        ]"
    ]
  where
    tag :: TB.Builder
    tag = TB.fromText t

    hash :: TB.Builder
    hash = asText (_payloadWithOutputsPayloadHash p)

    minerData :: TB.Builder
    minerData = asText (_payloadWithOutputsMiner p)

    coinbase :: TB.Builder
    coinbase = asText (_payloadWithOutputsCoinbase p)

tuple :: HasTextRepresentation a => HasTextRepresentation b => (a, b) -> TB.Builder
tuple (a, b) = "(" <> asText a <> ", " <> asText b <> ")"

asText :: HasTextRepresentation a => a -> TB.Builder
asText x = "unsafeFromText \"" <> TB.fromText (toText x) <> "\""

sep :: Foldable l => TB.Builder -> (a -> TB.Builder) -> l a -> TB.Builder
sep s f = go . toList
  where
    go [] = mempty
    go [h] = f h
    go (h:t) = f h <> s <> go t

------------------------------------------------------
-- Transaction Generation for coin v2 and remediations
------------------------------------------------------
