{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
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
  ) where

import Control.Monad.Except (throwError)

import Data.Aeson (encode)
import Data.Text (Text)

import Rosetta

import Servant.API
import Servant.Server

-- internal modules

import Chainweb.RestAPI.Utils (ChainwebEndpoint(..), Reassoc)
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

-- TODO: Investigate if Rosetta Erros can be dynamic
data RosettaFailure
    = RosettaChainUnspecified
    | RosettaInvalidChain Text
    | RosettaMempoolBadTx
    | RosettaInvalidBlockchainName Text
    | RosettaMismatchNetworkName ChainwebVersion Text
    | RosettaUnparsableTx
    | RosettaInvalidTx
    deriving (Show)

instance Enum RosettaFailure where
  succ RosettaChainUnspecified = RosettaInvalidChain defChainIdErrMsg
  succ (RosettaInvalidChain _) = RosettaMempoolBadTx
  succ RosettaMempoolBadTx = RosettaInvalidBlockchainName defBlockchainNameErrMsg
  succ (RosettaInvalidBlockchainName _) = RosettaMismatchNetworkName defChainwebVerErrMsg defNetworkNameErrMsg
  succ (RosettaMismatchNetworkName _ _) = RosettaUnparsableTx
  succ RosettaUnparsableTx = RosettaInvalidTx
  succ RosettaInvalidTx = errorWithoutStackTrace "Prelude.Enum.Bool.succ: bad argument"

  pred RosettaInvalidTx = RosettaUnparsableTx
  pred RosettaUnparsableTx = RosettaMismatchNetworkName defChainwebVerErrMsg defNetworkNameErrMsg
  pred (RosettaMismatchNetworkName _ _) = RosettaInvalidBlockchainName defBlockchainNameErrMsg
  pred (RosettaInvalidBlockchainName _) = RosettaMempoolBadTx
  pred RosettaMempoolBadTx = RosettaInvalidChain defChainIdErrMsg
  pred (RosettaInvalidChain _) = RosettaChainUnspecified
  pred RosettaChainUnspecified = errorWithoutStackTrace "Prelude.Enum.Bool.pred: bad argument"

  toEnum x
    | x == 0 = RosettaChainUnspecified
    | x == 1 = RosettaInvalidChain defChainIdErrMsg
    | x == 2 = RosettaMempoolBadTx
    | x == 3 = RosettaInvalidBlockchainName defBlockchainNameErrMsg
    | x == 4 = RosettaMismatchNetworkName defChainwebVerErrMsg defNetworkNameErrMsg
    | x == 5 = RosettaUnparsableTx
    | x == 6 = RosettaInvalidTx
    | otherwise = errorWithoutStackTrace "Prelude.Enum.().toEnum: bad argument"

  fromEnum RosettaChainUnspecified = 0
  fromEnum (RosettaInvalidChain _) = 1
  fromEnum RosettaMempoolBadTx = 2
  fromEnum (RosettaInvalidBlockchainName _) = 3
  fromEnum (RosettaMismatchNetworkName _ _) = 4
  fromEnum RosettaUnparsableTx = 5
  fromEnum RosettaInvalidTx = 6

-- NOTE: Must update when new rosetta errors are added
instance Bounded RosettaFailure where
  minBound = RosettaChainUnspecified
  maxBound = RosettaInvalidTx

defChainIdErrMsg :: Text
defChainIdErrMsg = "someInvalidChainId"

defBlockchainNameErrMsg :: Text
defBlockchainNameErrMsg = "someInvalidBlockchainName"

defChainwebVerErrMsg :: ChainwebVersion
defChainwebVerErrMsg = Mainnet01

defNetworkNameErrMsg :: Text
defNetworkNameErrMsg = "someInvalidNetworkName"

-- TODO: Better grouping of rosetta error index
rosettaError :: RosettaFailure -> RosettaError
rosettaError RosettaChainUnspecified = RosettaError 0 "No SubNetwork (chain) specified" False
rosettaError (RosettaInvalidChain cid) = RosettaError 1 ("Invalid chain value: " <> cid) False
rosettaError RosettaMempoolBadTx = RosettaError 2 "Transaction not present in mempool" False
rosettaError (RosettaInvalidBlockchainName a) = RosettaError 3 ("Invalid blockchain name: " <> a) False
rosettaError (RosettaMismatchNetworkName v a) = RosettaError 4
  ("Network name mismatch: expected " <> (chainwebVersionToText v) <> " but received " <> a) False
rosettaError RosettaUnparsableTx = RosettaError 5 "Transaction not parsable" False
rosettaError RosettaInvalidTx = RosettaError 6 "Invalid transaction" False

throwRosetta :: RosettaFailure -> Handler a
throwRosetta e = throwError err500 { errBody = encode $ rosettaError e }
