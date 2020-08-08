{-# LANGUAGE DataKinds #-}
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
module Ea ( main, genTxModules, gen20ChainPayloads ) where

import Control.Lens (set)

import Data.CAS.RocksDB
import Data.Foldable
import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Traversable
import Data.Tuple.Strict
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml

import Ea.Genesis

import System.IO.Temp
import System.LogLevel (LogLevel(..))

import Text.Printf

-- internal modules

import Chainweb.BlockHeaderDB
import Chainweb.Logger (genericLogger)
import Chainweb.Miner.Pact (noMiner)
import Chainweb.Pact.Backend.Utils
import Chainweb.Pact.PactService
import Chainweb.Pact.Types (defaultPactServiceConfig)
import Chainweb.Pact.Utils (toTxCreationTime)
import Chainweb.Payload.PayloadStore.InMemory
import Chainweb.Time
import Chainweb.Transaction
    (ChainwebTransaction, chainwebPayloadCodec, mkPayloadWithText)
import Chainweb.Utils
import Chainweb.Version (ChainwebVersion(..))
import Chainweb.Version.Utils (someChainId)

import Pact.ApiReq
import Pact.Types.ChainMeta
import Pact.Types.Command hiding (Payload)

---

main :: IO ()
main = void $ do
    devnet
    fastnet
    testnet
    mainnet
    putStrLn "Done."
  where
    devnet = mkPayloads
      [ development0
      , developmentN
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
      ]

show_ :: GChainId -> String
show_ = \case
    N -> "all chains"
    KAD -> "chains 10-19"
    n -> "Chain " <> show n

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
    -- coin contract genesis txs
    cc :: [FilePath]
    cc = [fungibleAssetV1, coinContractV1, gasPayer]
    -- final tx list.
    -- NB: this is position-sensitive data.
    txs :: [FilePath]
    txs = cc <> toList ns <> toList k <> toList a <> toList c


gen20ChainPayloads :: IO ()
gen20ChainPayloads = traverse_ mk20ChainPayload [developmentKAD, mainnetKAD]
  where
    mk20ChainPayload (Genesis v tag cid c k a ns) = do

      ((ccAr,ccCode,_,_),_) <- mkApiReq coinContractV2
      v2Install <- TIO.readFile coinContractV2Install
      let ccCode' = ccCode <> v2Install
          ccAr' = ccAr
            { _ylCode = Just ccCode'
            , _ylCodeFile = Nothing
            , _ylNonce = Just "coin-contract-v2-temp"
            }
      (_,ccTx) <- mkApiReqCmd False coinContractV2 ccAr'

      fa1 <- mkTx fungibleAssetV1
      fa2 <- mkTx fungibleAssetV2
      gp <- mkTx gasPayer

      txs <- ([fa1,fa2,ccTx,gp] ++) <$>
             mapM mkTx (toList ns <> toList k <> toList a <> toList c)
      cwTxs <- mkChainwebTxs' txs


      printf ("Generating Genesis 20-chain payload for %s on " <> show_ cid <> "...\n") $ show v
      genPayloadModule' v (tag <> sshow cid) cwTxs

---------------------
-- Payload Generation
---------------------

genPayloadModule :: ChainwebVersion -> Text -> [FilePath] -> IO ()
genPayloadModule v tag txFiles = genPayloadModule' v tag =<< mkChainwebTxs txFiles

genPayloadModule' :: ChainwebVersion -> Text -> [ChainwebTransaction] -> IO ()
genPayloadModule' v tag cwTxs =
    withTempRocksDb "chainweb-ea" $ \rocks ->
    withBlockHeaderDb rocks v cid $ \bhdb -> do
        let logger = genericLogger Warn TIO.putStrLn
        pdb <- newPayloadDb
        withSystemTempDirectory "ea-pact-db" $ \pactDbDir -> do
            T2 payloadWO _ <- withSqliteDb cid logger pactDbDir False $ \env ->
                initPactService' v cid logger bhdb pdb env defaultPactServiceConfig $
                    execNewGenesisBlock noMiner (V.fromList cwTxs)

            let payloadYaml = TE.decodeUtf8 $ Yaml.encode payloadWO
                modl = T.unlines $ startModule tag <> [payloadYaml] <> endModule
                fileName = "src/Chainweb/BlockHeader/Genesis/" <> tag <> "Payload.hs"

            TIO.writeFile (T.unpack fileName) modl
  where
    cid = someChainId v


startModule :: Text -> [Text]
startModule tag =
    [ "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE QuasiQuotes #-}"
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

mkTx :: FilePath -> IO (Command Text)
mkTx yamlFile = snd <$> mkApiReq yamlFile

mkChainwebTxs :: [FilePath] -> IO [ChainwebTransaction]
mkChainwebTxs txFiles = mkChainwebTxs' =<< traverse mkTx txFiles

mkChainwebTxs' :: [Command Text] -> IO [ChainwebTransaction]
mkChainwebTxs' rawTxs = do
  forM rawTxs $ \cmd -> do
    let cmdBS = fmap TE.encodeUtf8 cmd
        procCmd = verifyCommand cmdBS
    case procCmd of
      f@ProcFail{} -> fail (show f)
      ProcSucc c -> do
        let t = toTxCreationTime (Time (TimeSpan 0))
        return $! mkPayloadWithText <$> (c & setTxTime t & setTTL (TTLSeconds $ 2 * 24 * 60 * 60))
  where
    setTxTime = set (cmdPayload . pMeta . pmCreationTime)
    setTTL = set (cmdPayload . pMeta . pmTTL)

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
      encTx = encodeB64UrlNoPaddingText . codecEncode chainwebPayloadCodec
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
    , ""
    , "import Chainweb.Transaction"
    , "import Chainweb.Utils"
    , ""
    , "transactions :: IO [ChainwebTransaction]"
    , "transactions ="
    , "  let decodeTx t ="
    , "        fromEitherM . (first (userError . show)) . codecDecode chainwebPayloadCodec =<< decodeB64UrlNoPaddingText t"
    , "  in mapM decodeTx ["
    ]

endTxModule :: [Text]
endTxModule =
    [ "    ]"
    ]
