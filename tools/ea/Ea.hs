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
-- Eru Ilúvatar spoke "Eä", creating the heavens and the earth.
-- Eä means "to be" in Quenya, the ancient language of Tolkien's elves.
--
module Main ( main ) where

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

import Options.Generic

import System.LogLevel (LogLevel(..))

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (genesisTime)
import Chainweb.ChainId (unsafeChainId)
import Chainweb.Logger (genericLogger)
import Chainweb.Miner.Genesis (mineGenesis)
import Chainweb.Pact.PactService
import Chainweb.Pact.Types
import Chainweb.Time (Time(..), TimeSpan(..))
import Chainweb.Transaction (PayloadWithText(..))
import Chainweb.Utils (sshow)
import Chainweb.Version
    (ChainwebVersion(..), chainwebVersionFromText, chainwebVersionToText)

import Pact.ApiReq (mkApiReq)
import Pact.Types.Command hiding (Payload)
import Pact.Types.Runtime (noSPVSupport)

---

data Env w
    = Headers
        { version :: w ::: Text
          <?> "The ChainwebVersion to use."
        , chains :: w ::: Word16
          <?> "The number of genesis blocks to  to produce a genesis for."
        , time :: w ::: Maybe Int64
          <?> "Genesis Block Time, in microseconds since the Epoch. Default is the Genesis Time of the given ChainwebVersion." }
    | Payload
        { version :: w ::: Text
          <?> "The ChainwebVersion to use."
        , transactions :: w ::: [FilePath]
          <?> "Path to a YAML file containing a Pact transaction. Can be used multiple times. Default loads files based on the given ChainwebVersion." }
    deriving (Generic)

instance ParseRecord (Env Wrapped)

main :: IO ()
main = unwrapRecord "ea" >>= \case
    Headers v0 cs t -> do
        v <- chainwebVersionFromText v0
        let crtm = maybe (genesisTime v) (BlockCreationTime . Time . TimeSpan) t
            modl = headerModule v $ headers v cs crtm
            file = "src/Chainweb/BlockHeader/Genesis/" <> moduleName v <> ".hs"
        TIO.writeFile (T.unpack file) modl
        putStrLn $ "Generated Genesis BlockHeaders for " <> show v
    Payload v0 txs0 -> do
        v <- chainwebVersionFromText v0
        let txs = bool txs0 [defCoinContract, defGrants] $ null txs0
        genPayloadModule v txs
        putStrLn $ "Generated Genesis Payload for " <> show v
        putStrLn "Please recompile Chainweb and Ea before using the 'headers' command."

defCoinContract :: FilePath
defCoinContract = "pact/coin-contract/load-coin-contract.yaml"

defGrants :: FilePath
defGrants = "pact/genesis/testnet00/grants.yaml"

moduleName :: ChainwebVersion -> Text
moduleName = T.toTitle . chainwebVersionToText

--------------------
-- Header Generation
--------------------

-- | Given a number of Genesis `BlockHeader`s to generate, do just that.
headers :: ChainwebVersion -> Word16 -> BlockCreationTime -> [BlockHeader]
headers v cs ct = take (fromIntegral cs) $ map f [0..]
  where
    f cid = mineGenesis v (unsafeChainId cid) ct (Nonce 0)

headerModule :: ChainwebVersion -> [BlockHeader] -> Text
headerModule v hs = T.unlines $
    [ "{-# LANGUAGE QuasiQuotes #-}"
    , ""
    , "-- This module is auto-generated. DO NOT EDIT IT MANUALLY."
    , ""
    , "module Chainweb.BlockHeader.Genesis." <> moduleName v <> " where"
    , ""
    , "import Data.Text (Text)"
    , "import Data.Text.Encoding (encodeUtf8)"
    , "import Data.Yaml (decodeThrow)"
    , ""
    , "import GHC.Stack (HasCallStack)"
    , ""
    , "import NeatInterpolation (text)"
    , ""
    , "import Chainweb.BlockHeader"
    , "import Chainweb.Utils (fromJuste)"
    , ""
    , "unsafeFromYamlText :: HasCallStack => Text -> BlockHeader"
    , "unsafeFromYamlText = _objectEncoded . fromJuste . decodeThrow . encodeUtf8"
    , ""
    ] <> map (genesisHeader v) (zip [0..] hs)

genesisHeader :: ChainwebVersion -> (Int, BlockHeader) -> Text
genesisHeader v (n, h) = T.unlines
    [ fname <> " :: BlockHeader"
    , fname <> " = unsafeFromYamlText"
    , "    [text|"
    , TE.decodeUtf8 . Yaml.encode $ ObjectEncoded h
    , "    |]"
    ]
  where
    fname = chainwebVersionToText v <> "C" <> sshow n

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

    payloadWO <- initPactService' Testnet00 (unsafeChainId 0) logger noSPVSupport $
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
