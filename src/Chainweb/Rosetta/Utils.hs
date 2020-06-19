{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

-- |
-- Module: Chainweb.Rosetta.Util
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Linda Ortega <linda@kadena.io>
-- Stability: experimental
--
--
module Chainweb.Rosetta.Utils where

import Data.Aeson
import Data.Decimal
import Data.Word (Word64)


import qualified Data.HashMap.Strict as HM
import qualified Data.Memory.Endian as BA
import qualified Data.Text as T

import Numeric.Natural

import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.Runtime (TxId(..), TxLog(..))
import Pact.Types.PactValue (PactValue(..))
import Pact.Types.Exp (Literal(..))

import Rosetta

-- internal modules

import Chainweb.BlockCreationTime (BlockCreationTime(..))
import Chainweb.BlockHash
import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.BlockHeight (BlockHeight(..))
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version

---

--------------------------------------------------------------------------------
-- Rosetta Helper Types --
--------------------------------------------------------------------------------

type CoinbaseTx chainwebTx = chainwebTx
type AccountLog = (T.Text, Decimal, Value)
type UnindexedOperation = (Word64 -> Operation)

data ChainwebOperationStatus = Successful | Remediation
  deriving (Enum, Bounded, Show)

data OperationType =
    CoinbaseReward
  | FundTx
  | GasPayment
  | TransferOrCreateAcct
  deriving (Enum, Bounded, Show)


--------------------------------------------------------------------------------
-- Functions to create Rosetta types --
--------------------------------------------------------------------------------

-- | If its the genesis block, Rosetta wants the parent block to be itself.
--   Otherwise, fetch the parent header from the block.
parentBlockId :: BlockHeader -> BlockId
parentBlockId bh
  | (bHeight == genesisHeight v cid) = blockId bh  -- genesis
  | otherwise = parent
  where
    bHeight = _blockHeight bh
    cid = _blockChainId bh
    v = _blockChainwebVersion bh
    parent = BlockId
      { _blockId_index = _height (pred $ _blockHeight bh)
      , _blockId_hash = blockHashToText (_blockParent bh)
      }

blockId :: BlockHeader -> BlockId
blockId bh = BlockId
  { _blockId_index = _height (_blockHeight bh)
  , _blockId_hash = blockHashToText (_blockHash bh)
  }

rosettaTransactionFromCmd :: Command a -> [Operation] -> Transaction
rosettaTransactionFromCmd cmd ops =
  Transaction
    { _transaction_transactionId = pactHashToTransactionId (_cmdHash cmd)
    , _transaction_operations = ops
    , _transaction_metadata = Nothing
    }

rosettaTransaction :: CommandResult a -> [Operation] -> Transaction
rosettaTransaction cr ops =
  Transaction
    { _transaction_transactionId = rkToTransactionId (_crReqKey cr)
    , _transaction_operations = ops
    , _transaction_metadata = txMeta
    }
  where
    -- Include information on related transactions (i.e. continuations)
    txMeta
      | enableMetaData =
          case _crContinuation cr of
            Nothing -> Nothing
            Just pe -> Just $ HM.fromList
              [("related-transaction", toJSON pe)] -- TODO: document, make nicer?
      | otherwise = Nothing


pactHashToTransactionId :: PactHash -> TransactionId
pactHashToTransactionId hsh = TransactionId $ hashToText $ toUntypedHash hsh

rkToTransactionId :: RequestKey -> TransactionId
rkToTransactionId rk = TransactionId $ requestKeyToB16Text rk

indexedOperations :: [UnindexedOperation] -> [Operation]
indexedOperations logs = zipWith (\f i -> f i) logs [(0 :: Word64)..]

operationStatus :: ChainwebOperationStatus -> OperationStatus
operationStatus s@Successful =
  OperationStatus
    { _operationStatus_status = sshow s
    , _operationStatus_successful = True
    }
operationStatus s@Remediation =
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
    , _operation_metadata = opMeta
    }
  where
    opMeta
      | enableMetaData = Just $ HM.fromList [("txId", toJSON txid)] -- TODO: document
      | otherwise = Nothing
    accountId = AccountId
      { _accountId_address = key
      , _accountId_subAccount = Nothing  -- assumes coin acct contract only
      , _accountId_metadata = accountIdMeta
      }
    accountIdMeta
      | enableMetaData = Just $ HM.fromList [("ownership", guard)]  -- TODO: document
      | otherwise = Nothing


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
-- Misc Helper Functions --
--------------------------------------------------------------------------------

-- BUG: validator throws error when writing (some?) unstructured json to db.
-- Disable filling in metadata for now.
enableMetaData :: Bool
enableMetaData = False

-- TODO: document
maxRosettaNodePeerLimit :: Natural
maxRosettaNodePeerLimit = 64

-- | Parse TxLog Value into fungible asset account columns
txLogToAccountInfo :: TxLog Value -> Maybe AccountLog
txLogToAccountInfo (TxLog _ key (Object row)) = do
  guard :: Value <- (HM.lookup "guard" row) >>= (hushResult . fromJSON)
  (PLiteral (LDecimal bal)) <- (HM.lookup "balance" row) >>= (hushResult . fromJSON)
  pure $! (key, bal, guard)
txLogToAccountInfo _ = Nothing

hushResult :: Result a -> Maybe a
hushResult (Success w) = Just w
hushResult (Error _) = Nothing

overwriteError :: a -> Either b c -> Either a c
overwriteError e (Left _) = Left e
overwriteError _ (Right r) = Right r

noteOptional :: a -> Either a (Maybe c) -> Either a c
noteOptional e (Right Nothing) = Left e
noteOptional _ (Right (Just c)) = pure c
noteOptional _ (Left oe) = Left oe
