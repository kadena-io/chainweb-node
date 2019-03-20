{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Yaml

import Options.Generic

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeader.Genesis (genesisTime)
import Chainweb.ChainId (testChainId)
import Chainweb.Miner.Genesis (mineGenesis)
import Chainweb.Pact.Backend.Types (Env'(..), PactDbState(..))
import Chainweb.Pact.PactService (mkPureState, toHashedLogTxOutput)
import Chainweb.Pact.TransactionExec
import Chainweb.Pact.Types
import Chainweb.Pact.Utils (toEnv')
import Chainweb.Payload
import Chainweb.Time (Time(..), TimeSpan(..))
import Chainweb.Utils (sshow)
import Chainweb.Version
    (ChainwebVersion, chainwebVersionFromText, chainwebVersionToText)

import Pact.ApiReq (mkApiReq)
import Pact.Interpreter (mkPureEnv)
import Pact.Types.Command hiding (Payload)
import Pact.Types.Logger (Logger, Loggers, alwaysLog, newLogger)
import Pact.Types.Persistence (TxId(TxId))
import Pact.Types.Server (CommandConfig(..), CommandState)

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
    Payload v0 txs -> do
        putStrLn "Not implemented yet"

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
    f cid = mineGenesis v (testChainId cid) ct (Nonce 0)

headerModule :: ChainwebVersion -> [BlockHeader] -> Text
headerModule v hs = fold $
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
genesisHeader v (n, h) = fold
    [ fname <> " :: BlockHeader"
    , fname <> " = unsafeFromYamlText"
    , "    [text|"
    , TE.decodeUtf8 . Yaml.encode $ ObjectEncoded h
    , "    |]"
    , ""
    ]
  where
    fname = chainwebVersionToText v <> "C" <> sshow n

---------------------
-- Payload Generation
---------------------

genPayloadModule :: ChainwebVersion -> IO ()
genPayloadModule v = do
    coinTx <- mkTx "pact/coin-contract/load-coin-contract.yaml"
    grantsTx <- mkTx "pact/genesis/testnet00/grants.yaml"
    (loggers, state) <- initPact
    cmdStateVar <- newMVar (_pdbsState state)
    env' <- toEnv' (_pdbsDbEnv state)

    let logger = newLogger loggers "GenPayload"

    (txs, _) <- foldM (go logger env' cmdStateVar) ([],Transactional (TxId 0)) [coinTx,grantsTx]

    let moduleName = T.toTitle (chainwebVersionToText v) <> "GenesisPayload"
        fileName = "src/Chainweb/BlockHeader/Genesis/" <> moduleName <> ".hs"

    TIO.writeFile (T.unpack fileName) $ payloadModule moduleName txs
  where
    go :: Logger
       -> Env'
       -> MVar CommandState
       -> ([(a, ByteString)], ExecutionMode)
       -> (a, Command Text)
       -> IO ([(a, ByteString)], ExecutionMode)
    go logger (Env' pactDbEnv) cmdStateVar (outs, prevEM) (inp, cmd) = do
        let procCmd = verifyCommand (TE.encodeUtf8 <$> cmd)

        parsedCmd <- case procCmd of
            f@ProcFail{} -> fail (show f)
            ProcSucc c -> pure c
        ((result,txLogs),newEM) <- applyGenesisCmd logger Nothing pactDbEnv cmdStateVar prevEM parsedCmd
        -- TODO this is a heuristic for a failed tx
        when (null txLogs) $ fail $ "transaction failed: " ++ show (cmd,result)
        let fullOut = FullLogTxOutput (_crResult result) txLogs
            hashedOut = toHashedLogTxOutput fullOut
        pure ((inp, encodeJSON hashedOut):outs, newEM)

-- | Generate the entire module.
payloadModule :: Text -> [(ByteString, ByteString)] -> Text
payloadModule moduleName txs =
    fold $ startModule moduleName <> [payloadYaml txs] <> endModule

payloadYaml :: [(ByteString, ByteString)] -> Text
payloadYaml txs = TE.decodeUtf8 $ Yaml.encode payloadWO
  where
    payloadWO = PayloadWithOutputs
        { _payloadWithOutputsTransactions = pairs
        , _payloadWithOutputsPayloadHash = _blockPayloadPayloadHash payload
        , _payloadWithOutputsTransactionsHash = _blockPayloadTransactionsHash payload
        , _payloadWithOutputsOutputsHash = _blockPayloadOutputsHash payload }
    payload = newBlockPayload pairs
    pairs = S.fromList . map (Transaction *** TransactionOutput) $ reverse txs

startModule :: Text -> [Text]
startModule moduleName =
    [ "{-# LANGUAGE QuasiQuotes #-}"
    , ""
    , "-- This module is auto-generated. DO NOT EDIT IT MANUALLY."
    , ""
    , "module Chainweb.BlockHeader.Genesis." <> moduleName <> " ( payloadBlock ) where"
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

initPact :: IO (Loggers, PactDbState)
initPact = do
    let conf = CommandConfig Nothing Nothing Nothing Nothing
        loggers = alwaysLog
    env <- mkPureEnv loggers
    state <- mkPureState env conf
    pure (loggers,state)

mkTx :: FilePath -> IO (ByteString,Command Text)
mkTx yamlFile = do
    (_,cmd) <- mkApiReq yamlFile
    pure (encodeJSON cmd,cmd)

encodeJSON :: ToJSON a => a -> ByteString
encodeJSON = BL.toStrict . encodePretty' (defConfig { confCompare = compare })
