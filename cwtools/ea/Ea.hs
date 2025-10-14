{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Chainweb.ChainId qualified as Chainweb
import Chainweb.Logger (genericLogger)
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.PactService
import Chainweb.Pact.Payload
import Chainweb.Pact.Payload.PayloadStore.InMemory
import Chainweb.Pact.Transaction qualified as Pact
import Chainweb.Pact.Types (defaultPactServiceConfig, GenesisConfig(..))
import Chainweb.Pact.Utils (toTxCreationTime)
import Chainweb.Pact.Validations (defaultMaxTTLSeconds)
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Foldable
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
import Pact.Core.ChainData
import Pact.Core.Command.Client
import Pact.Core.Command.Types
import Pact.Core.Command.Types qualified as Pact
import Pact.Core.StableEncoding
import Pact.JSON.Encode qualified as J
import System.IO.Temp
import System.LogLevel

---

main :: IO ()
main = do

    mapConcurrently_ id
      [ devnet
      -- , fastnet
      , instantnet
      , pact53Transitionnet
      , quirkedPact5Instantnet
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
    pact53Transitionnet = mkPayloads [pact53TransitionCPM0, pact53TransitionCPMN]
    quirkedPact5Instantnet = mkPayloads [quirkedPact5InstantCPM0, quirkedPact5InstantCPMN]

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
mkPayload gen@(Genesis v _ (ChainIdRange l u) c k a ns cc) = do
    -- printf ("Generating Genesis Payload for %s on " <> show_ cidr <> "...\n") $ show v
    payloadModules <- forConcurrently [l..u] $ \cid ->
        withVersion v $ genPayloadModule (fullGenesisTag gen) (unsafeChainId cid) =<< mkChainwebTxs txs
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

genPayloadModule :: HasVersion => Text -> Chainweb.ChainId -> [Pact.Transaction] -> IO Text
genPayloadModule tag cid cwTxs = do
    let logger = genericLogger Warn TIO.putStrLn
    pdb <- newPayloadDb
    withSystemTempDirectory "ea-pact-db" $ \pactDbDir -> runResourceT $ do
        readWriteSql <- withSqliteDb cid logger pactDbDir False
        roPool <- withReadSqlitePool cid pactDbDir
        serviceEnv <- withPactService cid Nothing mempty logger Nothing pdb roPool readWriteSql defaultPactServiceConfig GeneratingGenesis
        payloadWO <- liftIO $ execNewGenesisBlock logger serviceEnv (V.fromList cwTxs)
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
    , "import Chainweb.Pact.Payload"
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
