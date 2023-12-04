{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
module Ea
  ( main ) where

import Control.Exception
import Control.Lens

import Data.Foldable
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Traversable
import qualified Data.Vector as V
import GHC.Exts(the)

import System.IO.Temp
import System.LogLevel (LogLevel(..))

import Text.Printf

-- internal modules

import Chainweb.BlockHeaderDB
import Chainweb.Logger (genericLogger)
import Chainweb.Miner.Pact (noMiner)
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.PactService
import Chainweb.Pact.Types (testPactServiceConfig)
import Chainweb.Pact.Utils (toTxCreationTime)
import Chainweb.Pact.Validations (defaultMaxTTL)
import Chainweb.Payload
import Chainweb.Payload.PayloadStore.InMemory
import Chainweb.Storage.Table.RocksDB
import Chainweb.Time
import Chainweb.Transaction
    (ChainwebTransaction, chainwebPayloadCodec, mkPayloadWithTextOld)
import Chainweb.Utils
import Chainweb.Version

import Ea.Genesis

import Pact.ApiReq
import Pact.Types.ChainMeta
import Pact.Types.Command hiding (Payload)

---

main :: IO ()
main = void $ do
    devnet
    fastDevnet
    fastnet
    testnet
    mainnet
    genTxModules
    genCoinV3Payloads
    genCoinV4Payloads
    genCoinV5Payloads
    putStrLn "Done."
  where
    devnet = mkPayloads
      [ development0
      , developmentN
      , developmentKAD
      ]
    fastDevnet = mkPayloads
      [ fastDevelopment0
      , fastDevelopmentN
      ]
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
    printf ("Generating Genesis Payload for %s on " <> show_ cidr <> "...\n") $ show v
    payloadModules <- for [l..u] $ \cid ->
        genPayloadModule v (fullGenesisTag gen) (unsafeChainId cid) =<< mkChainwebTxs txs
    -- checks that the modules on each chain are the same
    evaluate $ the payloadModules
  where
    -- final tx list.
    -- NB: this is position-sensitive data.
    txs :: [FilePath]
    txs = cc <> toList ns <> toList k <> toList a <> toList c

genCoinV3Payloads :: IO ()
genCoinV3Payloads = genTxModule "CoinV3" [coinContractV3]

genCoinV4Payloads :: IO ()
genCoinV4Payloads = genTxModule "CoinV4"
  [ fungibleXChainV1
  , coinContractV4
  ]

genCoinV5Payloads :: IO ()
genCoinV5Payloads = genTxModule "CoinV5"
  [ coinContractV5
  ]

---------------------
-- Payload Generation
---------------------

genPayloadModule :: ChainwebVersion -> Text -> ChainId -> [ChainwebTransaction] -> IO Text
genPayloadModule v tag cid cwTxs =
    withTempRocksDb "chainweb-ea" $ \rocks ->
    withBlockHeaderDb rocks v cid $ \bhdb -> do
        let logger = genericLogger Warn TIO.putStrLn
        pdb <- newPayloadDb
        withSystemTempDirectory "ea-pact-db" $ \pactDbDir -> do
            T2 payloadWO _ <- withSqliteDb cid logger pactDbDir False $ \env ->
                withPactService v cid logger bhdb pdb env testPactServiceConfig $
                    execNewGenesisBlock noMiner (V.fromList cwTxs)
            return $ TL.toStrict $ TB.toLazyText $ payloadModuleCode tag payloadWO

mkChainwebTxs :: [FilePath] -> IO [ChainwebTransaction]
mkChainwebTxs txFiles = mkChainwebTxs' =<< traverse mkTx txFiles

mkChainwebTxs' :: [Command Text] -> IO [ChainwebTransaction]
mkChainwebTxs' rawTxs =
    forM rawTxs $ \cmd -> do
        let cmdBS = fmap TE.encodeUtf8 cmd
            -- TODO: Use the new `assertCommand` function.
            -- We want to delete `verifyCommand` at some point.
            -- It's not critical for Ea because WebAuthn signatures (which
            -- the new `assertCommand` knows how to handle) are not present
            -- in Genesis blocks.
            procCmd = verifyCommand cmdBS
        case procCmd of
            f@ProcFail{} -> fail (show f)
            ProcSucc c -> do
                let t = toTxCreationTime (Time (TimeSpan 0))
                return $! mkPayloadWithTextOld <$> (c & setTxTime t & setTTL defaultMaxTTL)
  where
    setTxTime = set (cmdPayload . pMeta . pmCreationTime)
    setTTL = set (cmdPayload . pMeta . pmTTL)

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

genTxModules :: IO ()
genTxModules = void $ do
    genDevTxs
    genMainnetTxs
    genOtherTxs
    gen20ChainRemeds
    putStrLn "Done."
  where
    gen tag remeds = genTxModule tag $ upgrades <> remeds
    genOtherTxs = gen "Other" []
    genDevTxs = gen "Development"
      ["pact/coin-contract/remediations/devother/remediations.yaml"]

    genMain :: Int -> IO ()
    genMain chain = gen ("Mainnet" <> sshow chain)
      ["pact/coin-contract/remediations/mainnet/remediations" <> show chain <> ".yaml"]

    genMainnetTxs = mapM_ genMain [0..9]

    gen20ChainRemeds = genTxModule "MainnetKAD"
      ["pact/coin-contract/remediations/mainnet/remediations20chain.yaml"]

    upgrades = [fungibleAssetV2, coinContractV2]

genTxModule :: Text -> [FilePath] -> IO ()
genTxModule tag txFiles = do
    putStrLn $ "Generating tx module for " ++ show tag
    cwTxs <- mkChainwebTxs txFiles

    let encTxs = map quoteTx cwTxs
        quoteTx tx = "    \"" <> encTx tx <> "\""
        encTx = encodeB64UrlNoPaddingText . codecEncode (chainwebPayloadCodec maxBound)
        modl = T.unlines $ startTxModule tag <> [T.intercalate "\n    ,\n" encTxs] <> endTxModule
        fileName = "src/Chainweb/Pact/Transactions/" <> tag <> "Transactions.hs"

    TIO.writeFile (T.unpack fileName) modl

startTxModule :: Text -> [Text]
startTxModule tag =
    [ "{-# LANGUAGE OverloadedStrings #-}"
    , ""
    , "-- This module is auto-generated. DO NOT EDIT IT MANUALLY."
    , ""
    , "module Chainweb.Pact.Transactions." <> tag <> "Transactions ( transactions ) where"
    , ""
    , "import Data.Bifunctor (first)"
    , "import System.IO.Unsafe"
    , ""
    , "import Chainweb.Transaction"
    , "import Chainweb.Utils"
    , ""
    , "transactions :: [ChainwebTransaction]"
    , "transactions ="
    , "  let decodeTx t ="
    , "        fromEitherM . (first (userError . show)) . codecDecode (chainwebPayloadCodec maxBound) =<< decodeB64UrlNoPaddingText t"
    , "  in unsafePerformIO $ mapM decodeTx ["
    ]

endTxModule :: [Text]
endTxModule =
    [ "    ]"
    ]
