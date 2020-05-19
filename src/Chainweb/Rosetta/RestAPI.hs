{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: Chainweb.Rosetta.RestAPI
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
--
module Chainweb.Rosetta.RestAPI
  ( -- * Endpoints
    RosettaApi_
  , RosettaApi
    -- * Errors
  , RosettaFailure(..)
  , rosettaError
  , throwRosetta
  , validateNetwork
  , kdaToRosettaAmount
  ) where

import Control.Error.Util
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Except (ExceptT)

import Data.Aeson (encode)
import Data.Decimal
import qualified Data.Text as T

import Rosetta

import Servant.API
import Servant.Server

import Text.Read (readMaybe)

-- internal modules

import Chainweb.RestAPI.Utils (ChainwebEndpoint(..), Reassoc)
import Chainweb.Utils
import Chainweb.Version

---

type RosettaApi_ =
    -- Accounts --
    "rosetta" :> "account" :> "balance"
        :> ReqBody '[JSON] AccountBalanceReq
        :> Post '[JSON] AccountBalanceResp
    -- Blocks --
    :<|> "rosetta" :> "block" :> "transaction"
        :> ReqBody '[JSON] BlockTransactionReq
        :> Post '[JSON] BlockTransactionResp
    :<|> "rosetta" :> "block"
        :> ReqBody '[JSON] BlockReq
        :> Post '[JSON] BlockResp
    -- Construction --
    :<|> "rosetta" :> "construction" :> "metadata"
        :> ReqBody '[JSON] ConstructionMetadataReq
        :> Post '[JSON] ConstructionMetadataResp
    :<|> "rosetta" :> "construction" :> "submit"
        :> ReqBody '[JSON] ConstructionSubmitReq
        :> Post '[JSON] ConstructionSubmitResp
    -- Mempool --
    :<|> "rosetta" :> "mempool" :> "transaction"
        :> ReqBody '[JSON] MempoolTransactionReq
        :> Post '[JSON] MempoolTransactionResp
    :<|> "rosetta" :> "mempool"
        :> ReqBody '[JSON] MempoolReq
        :> Post '[JSON] MempoolResp
    -- Network --
    :<|> "rosetta" :> "network" :> "list"
        :> ReqBody '[JSON] MetadataReq
        :> Post '[JSON] NetworkListResp
    :<|> "rosetta" :> "network" :> "options"
        :> ReqBody '[JSON] NetworkReq
        :> Post '[JSON] NetworkOptionsResp
    :<|> "rosetta" :> "network" :> "status"
        :> ReqBody '[JSON] NetworkReq
        :> Post '[JSON] NetworkStatusResp

type RosettaApi (v :: ChainwebVersionT) = 'ChainwebEndpoint v :> Reassoc RosettaApi_

-- TODO: Investigate if Rosetta Erros can be dynamic?
data RosettaFailure
    = RosettaChainUnspecified
    | RosettaInvalidChain
    | RosettaMempoolBadTx
    | RosettaUnparsableTx
    | RosettaInvalidTx
    | RosettaInvalidBlockchainName
    | RosettaMismatchNetworkName
    | RosettaHistBalCheckUnsupported
    | RosettaPactExceptionThrown
    | RosettaPactErrorThrown
    | RosettaExpectedBalDecimal
    | RosettaInvalidResultMetaData
    | RosettaSubAcctUnsupported
    | RosettaMismatchTxLogs
    | RosettaUnparsableTxLog
    | RosettaInvalidBlockHeight
    | RosettaBlockHashNotFound
    | RosettaUnparsableBlockHash
    | RosettaOrphanBlockHash
    | RosettaMismatchBlockHashHeight
    | RosettaPayloadNotFound
    | RosettaUnparsableTxOut
    deriving (Show, Enum, Bounded)


-- TODO: Better grouping of rosetta error index?
rosettaError :: RosettaFailure -> RosettaError
rosettaError RosettaChainUnspecified = RosettaError 0 "No SubNetwork (chain) specified" False
rosettaError RosettaInvalidChain = RosettaError 1 "Invalid chain value" False
rosettaError RosettaMempoolBadTx = RosettaError 2 "Transaction not present in mempool" False
rosettaError RosettaUnparsableTx = RosettaError 3 "Transaction not parsable" False
rosettaError RosettaInvalidTx = RosettaError 4 "Invalid transaction" False
rosettaError RosettaInvalidBlockchainName = RosettaError 5 "Invalid blockchain name" False
rosettaError RosettaMismatchNetworkName = RosettaError 6 "Invalid Chainweb network name" False
rosettaError RosettaHistBalCheckUnsupported =
  RosettaError 7 "Historical account balance lookup is not supported." False
rosettaError RosettaPactExceptionThrown =
  RosettaError 7 "A pact exception was thrown" False
rosettaError RosettaPactErrorThrown = RosettaError 8 "Transaction failed with a pact error" False
rosettaError RosettaExpectedBalDecimal = RosettaError 9 "Expected balance as a decimal" False
rosettaError RosettaInvalidResultMetaData = RosettaError 10 "Invalid meta data field in command result" False
rosettaError RosettaSubAcctUnsupported = RosettaError 11 "Sub account identifier is not supported" False
rosettaError RosettaMismatchTxLogs =
  RosettaError 12 "Unable to match transactions to transaction logs as expected" False
rosettaError RosettaUnparsableTxLog = RosettaError 13 "TxLogs not parsable" False
rosettaError RosettaInvalidBlockHeight = RosettaError 14 "Invalid block height" False -- TODO if retry could succeed
rosettaError RosettaBlockHashNotFound = RosettaError 15 "Block hash was not found" False
rosettaError RosettaUnparsableBlockHash = RosettaError 16 "Block hash not parsable" False
rosettaError RosettaOrphanBlockHash = RosettaError 17 "Block hash not in the latest fork" False
rosettaError RosettaMismatchBlockHashHeight = RosettaError 18 "Block hash and block height did not match" False
rosettaError RosettaPayloadNotFound = RosettaError 19 "Block payload not found" False
rosettaError RosettaUnparsableTxOut = RosettaError 20 "Transaction output not parsable" False


throwRosetta :: RosettaFailure -> Handler a
throwRosetta e = throwError err500 { errBody = encode $ rosettaError e }


-- | Every Rosetta request that requires a `NetworkId` also requires a
-- `SubNetworkId`, at least in the case of Chainweb.
validateNetwork :: Monad m => ChainwebVersion -> NetworkId -> ExceptT RosettaFailure m ChainId
validateNetwork v (NetworkId bc n msni) = do
    when (bc /= "kadena") $ throwError RosettaInvalidBlockchainName
    when (Just v /= fromText n) $ throwError RosettaMismatchNetworkName
    SubNetworkId cid _ <- msni ?? RosettaChainUnspecified
    readChainIdText v cid ?? RosettaInvalidChain


-- | Guarantees that the `ChainId` given actually belongs to this
-- `ChainwebVersion`.
readChainIdText :: ChainwebVersion -> T.Text -> Maybe ChainId
readChainIdText v c = do
  cid <- readMaybe @Word (T.unpack c)
  mkChainId v cid


kdaToRosettaAmount :: Decimal -> Amount
kdaToRosettaAmount k = Amount (sshow amount) currency Nothing
  where
    -- Value in atomic units represented as an arbitrary-sized signed integer.
    amount :: Integer
    amount = floor $ k * (realToFrac ((10 :: Integer) ^ numDecimals))

    -- How to convert from atomic units to standard units
    numDecimals = 12 :: Word

    currency = Currency "KDA" numDecimals Nothing
