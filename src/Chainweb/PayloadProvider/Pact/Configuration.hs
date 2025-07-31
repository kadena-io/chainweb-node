{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module: Chainweb.PayloadProvider.Pact.Configuration
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.PayloadProvider.Pact.Configuration
( PactProviderConfig(..)
, pactConfigReintroTxs
, pactConfigMempoolP2p
, pactConfigBlockGasLimit
, pactConfigLogGas
, pactConfigMinGasPrice
, pactConfigPactQueueSize
, pactConfigPreInsertCheckTimeout
, pactConfigAllowReadsInLocal
, pactConfigFullHistoricPactState
, pactConfigEnableLocalTimeout
, pactConfigMiner
, defaultPactProviderConfig
, pPactProviderConfig
, pMiner
, invalidMiner
) where

import Chainweb.Mempool.Mempool qualified as Mempool
import Chainweb.Mempool.P2pConfig
import Chainweb.Miner.Pact (Miner(..), MinerGuard(..), MinerId(..))
import Chainweb.Pact.Types (defaultPreInsertCheckTimeout)
import Chainweb.Payload (PayloadWithOutputs)
import Chainweb.Time hiding (second)
import Chainweb.Utils
import Chainweb.Version
import Configuration.Utils hiding (Error, Lens', disabled)
import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Data.Maybe
import Data.Set qualified as Set
import GHC.Generics hiding (from, to)
import Numeric.Natural (Natural)
import Pact.Core.Gas qualified as Pact
import Pact.Core.Guards qualified as Pact
import Pact.Core.StableEncoding
import Pact.JSON.Encode qualified as J
import Prelude hiding (log)

-- -------------------------------------------------------------------------- --

invalidMiner :: Miner
invalidMiner = Miner ""
    $ MinerGuard
    $ Pact.GKeyset (Pact.KeySet mempty Pact.KeysAll)

pMiner :: ChainId -> OptionParser Miner
pMiner cid = pkToMiner <$> pPk
  where
    pkToMiner pk = Miner
        (MinerId $ "k:" <> Pact._pubKey pk)
        (MinerGuard $ Pact.GKeyset $ Pact.KeySet (Set.singleton pk) Pact.KeysAll)
    pPk = Pact.PublicKeyText <$> textOption
        % prefixLongCid cid "pact-mining-public-key"
        <> helpCid cid
            "Public key of a miner in hex decimal encoding. The account name is the public key prefix by 'k:'. Payload production is disabled if not set."

-- -------------------------------------------------------------------------- --
-- Payload Provider Configuration

-- | Placeholder for PactProviderConfig
--
data PactProviderConfig = PactProviderConfig
    { _pactConfigReintroTxs :: !Bool
    , _pactConfigMempoolP2p :: !(EnableConfig MempoolP2pConfig)
    , _pactConfigBlockGasLimit :: !Mempool.GasLimit
    , _pactConfigLogGas :: !Bool
    , _pactConfigMinGasPrice :: !Mempool.GasPrice
    , _pactConfigPactQueueSize :: !Natural
    , _pactConfigPreInsertCheckTimeout :: !Micros
    , _pactConfigAllowReadsInLocal :: !Bool

    -- For shallow nodes this should be a history depth parameter
    , _pactConfigFullHistoricPactState :: !Bool

    , _pactConfigEnableLocalTimeout :: !Bool
    , _pactConfigMiner :: !(Maybe Miner)
    , _pactConfigDatabaseDirectory :: !(Maybe FilePath)
    , _pactConfigGenesisPayload :: !(Maybe PayloadWithOutputs)
    }
    deriving (Show, Eq, Generic)

makeLenses ''PactProviderConfig

instance ToJSON PactProviderConfig where
    toJSON o = object
        [ "reintroTxs" .= _pactConfigReintroTxs o
        , "mempoolP2p" .= _pactConfigMempoolP2p o
        , "gasLimitOfBlock" .= J.toJsonViaEncode (StableEncoding $ _pactConfigBlockGasLimit o)
        , "logGas" .= _pactConfigLogGas o
        , "minGasPrice" .= J.toJsonViaEncode (StableEncoding $ _pactConfigMinGasPrice o)
        , "pactQueueSize" .= _pactConfigPactQueueSize o
        , "preInsertCheckTimeout" .= _pactConfigPreInsertCheckTimeout o
        , "allowReadsInLocal" .= _pactConfigAllowReadsInLocal o
        , "fullHistoricPactState" .= _pactConfigFullHistoricPactState o
        , "enableLocalTimeout" .= _pactConfigEnableLocalTimeout o
        , "miner" .= J.toJsonViaEncode (_pactConfigMiner o)
        , "databaseDirectory" .= _pactConfigDatabaseDirectory o
        , "genesisPayload" .= _pactConfigGenesisPayload o
        ]

instance FromJSON (PactProviderConfig -> PactProviderConfig) where
    parseJSON = withObject "PactProviderConfig" $ \o -> id
        <$< pactConfigReintroTxs ..: "reintroTxs" % o
        <*< pactConfigMempoolP2p %.: "mempoolP2p" % o
        <*< pactConfigBlockGasLimit . iso StableEncoding _stableEncoding ..: "gasLimitOfBlock" % o
        <*< pactConfigLogGas ..: "logGas" % o
        <*< pactConfigMinGasPrice . iso StableEncoding _stableEncoding ..: "minGasPrice" % o
        <*< pactConfigPactQueueSize ..: "pactQueueSize" % o
        <*< pactConfigPreInsertCheckTimeout ..: "preInsertCheckTimeout" % o
        <*< pactConfigAllowReadsInLocal ..: "allowReadsInLocal" % o
        <*< pactConfigFullHistoricPactState ..: "fullHistoricPactState" % o
        <*< pactConfigEnableLocalTimeout ..: "enableLocalTimeout" % o
        <*< pactConfigMiner ..: "miner" % o
        <*< pactConfigDatabaseDirectory ..: "databaseDirectory" % o
        <*< pactConfigGenesisPayload ..: "genesisPayload" % o

defaultPactProviderConfig :: PactProviderConfig
defaultPactProviderConfig = PactProviderConfig
    { _pactConfigReintroTxs = True
    , _pactConfigMempoolP2p = defaultEnableConfig defaultMempoolP2pConfig
    , _pactConfigBlockGasLimit = Pact.GasLimit (Pact.Gas 150_000)
    , _pactConfigLogGas = False
    , _pactConfigMinGasPrice = Pact.GasPrice 1e-8
    , _pactConfigPactQueueSize = 2000
    , _pactConfigPreInsertCheckTimeout = defaultPreInsertCheckTimeout
    , _pactConfigAllowReadsInLocal = False
    , _pactConfigFullHistoricPactState = True
    , _pactConfigEnableLocalTimeout = False
    , _pactConfigMiner = Nothing
    , _pactConfigDatabaseDirectory = Nothing
    , _pactConfigGenesisPayload = Nothing
    }

pPactProviderConfig :: ChainId -> MParser PactProviderConfig
pPactProviderConfig cid = id
    <$< pactConfigReintroTxs .:: enableDisableFlag
        % prefixLongCid cid "pact-tx-reintro"
        <> helpCid cid "whether to enable transaction reintroduction from losing forks"
        <> internal
    <*< pactConfigMempoolP2p %:: pEnableConfigCid "pact-mempool-p2p" cid pMempoolP2pConfig
    <*< pactConfigBlockGasLimit . iso StableEncoding _stableEncoding .:: jsonOption
        % prefixLongCid cid "pact-block-gas-limit"
        <> helpCid cid "the sum of all transaction gas fees in a block must not exceed this number"
    <*< pactConfigLogGas .:: boolOption_
        % prefixLongCid cid "pact-log-gas"
        <> helpCid cid "log gas consumed by Pact commands"
    <*< pactConfigMinGasPrice . iso StableEncoding _stableEncoding .:: jsonOption
        % prefixLongCid cid "pact-min-gas-price"
        <> helpCid cid "the gas price of an individual transaction in a block must not be beneath this number"
    <*< pactConfigPactQueueSize .:: jsonOption
        % prefixLongCid cid "pact-queue-size"
        <> helpCid cid "max size of pact internal queue"
    <*< pactConfigPreInsertCheckTimeout .:: jsonOption
        % prefixLongCid cid "pact-pre-insert-check-timeout"
        <> helpCid cid "Max allowed time in microseconds for the transactions validation in the PreInsertCheck command."
        <> internal
    <*< pactConfigAllowReadsInLocal .:: boolOption_
        % prefixLongCid cid "pact-allowReadsInLocal"
        <> helpCid cid "Enable direct database reads of smart contract tables in local queries."
    <*< pactConfigFullHistoricPactState .:: boolOption_
        % prefixLongCid cid "pact-full-historic-pact-state"
        <> helpCid cid "Write full historic Pact state; only enable for custodial or archival nodes."
    <*< pactConfigEnableLocalTimeout .:: option auto
        % prefixLongCid cid "pact-enable-local-timeout"
        <> helpCid cid "Enable timeout support on /local endpoints"
    <*< pactConfigMiner .:: fmap Just % pMiner cid
    <*< pactConfigDatabaseDirectory .:: fmap Just % fileOption
        % prefixLongCid cid "pact-database-directory"
        <> helpCid cid "the directory to store the pact database"
