{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Rosetta.Util
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Linda Ortega <linda@kadena.io>
-- Stability: experimental
--
--
module Chainweb.Rosetta.Util where

import Control.Error.Util
import Control.Lens ((^?))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Map (Map)
import Data.Decimal
import Data.CAS
import Data.Word (Word64)


import qualified Data.ByteString.Short as BSS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Memory.Endian as BA
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.Vector as V

import Numeric.Natural

import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.Runtime (TxId(..), TxLog(..), Domain(..))
import Pact.Types.PactValue (PactValue(..))
import Pact.Types.Exp (Literal(..))

import Rosetta
import Servant.Server

-- internal modules

import Chainweb.BlockCreationTime (BlockCreationTime(..))
import Chainweb.BlockHash
import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Mempool.Mempool (TransactionHash(..))
import Chainweb.Pact.Service.Types (Domain'(..), BlockTxHistory(..))
import Chainweb.Payload hiding (Transaction(..))
import Chainweb.Payload.PayloadStore
import Chainweb.Rosetta.RestAPI
import Chainweb.Time
import Chainweb.TreeDB (seekAncestor)
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebPactExecutionService (PactExecutionService(..))

---

--------------------------------------------------------------------------------
-- Rosetta Helper Types

type CoinbaseCommandResult = CommandResult Hash
type AccountLog = (T.Text, Decimal, Value)
type UnindexedOperation = (Word64 -> Operation)

data ChainwebOperationStatus = Successful
  deriving (Enum, Bounded, Show)

data OperationType =
    CoinbaseReward
  | FundTx
  | GasPayment
  | TransferOrCreateAcct
  deriving (Enum, Bounded, Show)

--------------------------------------------------------------------------------
-- Functions to create Rosetta types

-- | If its the genesis block, Rosetta wants the parent block to be itself.
--   Otherwise, fetch the parent header from the block.
parentBlockId :: BlockHeader -> BlockId
parentBlockId bh
  | (_blockHeight bh == 0) = blockId bh  -- genesis
  | otherwise = parent
  where parent = BlockId
          { _blockId_index = _height (pred $ _blockHeight bh)
          , _blockId_hash = blockHashToText (_blockParent bh)
          }


blockId :: BlockHeader -> BlockId
blockId bh = BlockId
  { _blockId_index = _height (_blockHeight bh)
  , _blockId_hash = blockHashToText (_blockHash bh)
  }


rosettaTransaction :: CommandResult Hash -> [Operation] -> Transaction
rosettaTransaction cr ops =
  Transaction
    { _transaction_transactionId = rkToTransactionId (_crReqKey cr)
    , _transaction_operations = ops
    , _transaction_metadata = txMeta
    }
  where
    -- Include information on related transactions (i.e. continuations)
    txMeta = case _crContinuation cr of
      Nothing -> Nothing
      Just pe -> Just $ HM.fromList [("related-transaction", toJSON pe)] -- TODO: document, nicer?


-- TODO: standardize reading/creating TransactionId
rkToTransactionId :: RequestKey -> TransactionId
rkToTransactionId rk = TransactionId $ requestKeyToB16Text rk

fromTransactionId :: TransactionId -> TransactionHash
fromTransactionId (TransactionId ti) = TransactionHash . BSS.toShort $ T.encodeUtf8 ti


indexedOperations :: [UnindexedOperation] -> [Operation]
indexedOperations logs = zipWith (\f i -> f i) logs [(0 :: Word64)..]


-- TODO: Investigate how continuations affect amounts in coin table
-- Convert chainweb operation status enum to Rosetta operation status
operationStatus :: ChainwebOperationStatus -> OperationStatus
operationStatus s@Successful =
  OperationStatus
    { _operationStatus_status = sshow s
    , _operationStatus_successful = True
    }


operation
    :: ChainwebOperationStatus
    -> OperationType
    -> TxId
    -> AccountLog
    -> Word64
    -> Operation
operation ostatus otype txid (key, bal, guard) idx =
  Operation
    { _operation_operationId = OperationId idx Nothing
    , _operation_relatedOperations = Nothing -- TODO: implement
    , _operation_type = sshow otype
    , _operation_status = sshow ostatus
    , _operation_account = Just accountId
    , _operation_amount = Just $ kdaToRosettaAmount bal
    , _operation_metadata = Just $ HM.fromList [("txId", toJSON txid)] -- TODO: document
    }
  where
    accountId = AccountId
      { _accountId_address = key
      , _accountId_subAccount = Nothing  -- assumes coin acct contract only
      , _accountId_metadata = Just accountIdMeta
      }
    accountIdMeta = HM.fromList [("ownership", guard)]  -- TODO: document


-- Timestamp of the block in milliseconds since the Unix Epoch.
-- NOTE: Chainweb provides this timestamp in microseconds.
rosettaTimestamp :: BlockHeader -> Word64
rosettaTimestamp bh = BA.unLE . BA.toLE $ fromInteger msTime
  where
    msTime = int $ microTime `div` ms
    TimeSpan ms = millisecond
    microTime = encodeTimeToWord64 $ _bct (_blockCreationTime bh)

kdaToRosettaAmount :: Decimal -> Amount
kdaToRosettaAmount k = Amount (sshow amount) currency Nothing
  where
    -- Value in atomic units represented as an arbitrary-sized signed integer.
    amount :: Integer
    amount = floor $ k * (realToFrac ((10 :: Integer) ^ numDecimals))

    -- How to convert from atomic units to standard units
    numDecimals = 12 :: Word

    currency = Currency "KDA" numDecimals Nothing

--------------------------------------------------------------------------------
-- Chainweb Helper Functions

getLatestBlockHeader
    :: CutDb cas
    -> ChainId
    -> ExceptT RosettaFailure Handler BlockHeader
getLatestBlockHeader cutDb cid = do
  c <- liftIO $ _cut cutDb
  HM.lookup cid (_cutMap c) ?? RosettaInvalidChain


findBlockHeaderInCurrFork
    :: CutDb cas
    -> ChainId
    -> Maybe Word64
    -- ^ Block Height
    -> Maybe T.Text
    -- ^ Block Hash
    -> ExceptT RosettaFailure Handler BlockHeader
findBlockHeaderInCurrFork cutDb cid someHeight someHash = do
  latestBlock <- getLatestBlockHeader cutDb cid
  chainDb <- (cutDb ^? cutDbBlockHeaderDb cid) ?? RosettaInvalidChain

  case (someHeight, someHash) of
    (Nothing, Nothing) -> pure latestBlock
    (Just hi, Nothing) -> byHeight chainDb latestBlock hi
    (Just hi, Just hsh) -> do
      bh <- byHeight chainDb latestBlock hi
      bhashExpected <- (blockHashFromText hsh) ?? RosettaUnparsableBlockHash
      if (_blockHash bh == bhashExpected)
        then pure bh
        else throwError RosettaMismatchBlockHashHeight
    (Nothing, Just hsh) -> do
      bhash <- (blockHashFromText hsh) ?? RosettaUnparsableBlockHash
      somebh <- liftIO $ (casLookup chainDb bhash)
      bh <- somebh ?? RosettaBlockHashNotFound
      isInCurrFork <- liftIO $ memberOfHeader cutDb cid bhash latestBlock
      if isInCurrFork
        then pure bh
        else throwError RosettaOrphanBlockHash
  where
    byHeight db latest hi = do
      somebh <- liftIO $ seekAncestor db latest (int hi)
      somebh ?? RosettaInvalidBlockHeight


getBlockOutputs
    :: forall cas
    . PayloadCasLookup cas
    => PayloadDb cas
    -> BlockHeader
    -> ExceptT RosettaFailure Handler (CoinbaseCommandResult, V.Vector (CommandResult Hash))
getBlockOutputs payloadDb bh = do
  someOut <- liftIO $ casLookup payloadDb (_blockPayloadHash bh)
  outputs <- someOut ?? RosettaPayloadNotFound
  txsOut <- decodeTxsOut outputs ?? RosettaUnparsableTxOut
  coinbaseOut <- decodeCoinbaseOut outputs ?? RosettaUnparsableTxOut
  pure $ (coinbaseOut, txsOut)

  where
    decodeCoinbaseOut :: PayloadWithOutputs -> Maybe (CommandResult Hash)
    decodeCoinbaseOut = decodeStrictOrThrow . _coinbaseOutput . _payloadWithOutputsCoinbase

    decodeTxsOut :: PayloadWithOutputs -> Maybe (V.Vector (CommandResult Hash))
    decodeTxsOut pwo = mapM (decodeStrictOrThrow . _transactionOutputBytes . snd)
                       (_payloadWithOutputsTransactions pwo)

getTxLogs
    :: PactExecutionService
    -> BlockHeader
    -> ExceptT RosettaFailure Handler (Map TxId [AccountLog])
getTxLogs cr bh = do
  someHist <- liftIO $ (_pactBlockTxHistory cr) bh d
  (BlockTxHistory hist) <- (hush someHist) ?? RosettaPactExceptionThrown
  case (M.size hist) of
    0 -> throwError RosettaInvalidChain
    _ -> pure ()
  let histParsed = M.mapMaybe (mapM txLogToAccountInfo) hist
  if (M.size histParsed == M.size hist)
    then pure histParsed
    else throwError RosettaUnparsableTxLog
  where
    d = (Domain' (UserTables "coin_coin-table"))

--------------------------------------------------------------------------------
-- Misc Helper Functions

-- TODO: document
maxRosettaNodePeerLimit :: Natural
maxRosettaNodePeerLimit = 64

txLogToAccountInfo :: TxLog Value -> Maybe AccountLog
txLogToAccountInfo (TxLog _ key (Object row)) = do
  guard :: Value <- (HM.lookup "guard" row) >>= (hushResult . fromJSON)
  (PLiteral (LDecimal bal)) <- (HM.lookup "balance" row) >>= (hushResult . fromJSON)
  pure $ (key, bal, guard)
txLogToAccountInfo _ = Nothing

hushResult :: Result a -> Maybe a
hushResult (Success w) = Just w
hushResult (Error _) = Nothing
