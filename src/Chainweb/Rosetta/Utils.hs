{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Rosetta.Utils
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Linda Ortega <linda@kadena.io>
-- Stability: experimental
--
--
module Chainweb.Rosetta.Utils where

import Data.Aeson
import Data.Decimal
import Data.List (sortOn, inits)
import Data.Word (Word64)
import Text.Read (readMaybe)

import qualified Data.HashMap.Strict as HM
import qualified Data.Memory.Endian as BA
import qualified Data.Text as T
import qualified Pact.Types.Runtime as P

import Numeric.Natural

import Pact.Types.Command
import Pact.Types.Hash
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
-- Rosetta Metadata Types --
--------------------------------------------------------------------------------

class ToObject a where
  toPairs :: a -> [(T.Text, Value)]
  toObject :: a -> Object

data OperationMetaData = OperationMetaData
  { _operationMetaData_txId :: !P.TxId
  , _operationMetaData_totalBalance :: !Amount
  , _operationMetaData_prevOwnership :: !Value
  } deriving Show
-- TODO: document
instance ToObject OperationMetaData where
  toPairs (OperationMetaData txId bal prevOwnership) =
    [ "tx-id" .= txId
    , "total-balance" .= bal
    , "prev-owenership" .= prevOwnership ]
  toObject opMeta = HM.fromList (toPairs opMeta)

newtype AccountIdMetaData = AccountIdMetaData
  { _accountIdMetaData_currOwnership :: Value }
  deriving Show
-- TODO: document
instance ToObject AccountIdMetaData where
  toPairs (AccountIdMetaData currOwnership) =
    [ "current-ownership" .= currOwnership ]
  toObject acctMeta = HM.fromList (toPairs acctMeta)

-- Continuation MetaDatas
--
data ContinuationCurrStep = ContinuationCurrStep
  { _continuationCurrStep_chainId :: !T.Text
  , _continuationCurrStep_stepId :: !Int
  -- ^ Step that was executed or skipped
  , _continuationCurrStep_hasRollback :: Bool
  -- ^ Track whether a current step has a rollback
  } deriving Show
-- TODO: document
instance ToObject ContinuationCurrStep where
  toPairs (ContinuationCurrStep cid step rollback) =
    [ "chain-id" .= cid
    , "step-id" .= step
    , "has-rollback" .= rollback ]
  toObject contCurrStep = HM.fromList (toPairs contCurrStep)

toContStep :: ChainId -> P.PactExec -> ContinuationCurrStep
toContStep cid pe = ContinuationCurrStep
  { _continuationCurrStep_chainId = chainIdToText cid
  , _continuationCurrStep_stepId = P._peStep pe
  , _continuationCurrStep_hasRollback = P._peStepHasRollback pe
  }


newtype ContinuationNextStep = ContinuationNextStep
  { _continuationNextStep_chainId :: T.Text
  } deriving Show
-- TODO: document
instance ToObject ContinuationNextStep where
  toPairs (ContinuationNextStep cid) = [ "target-chain-id" .= cid ]
  toObject contNextStep = HM.fromList (toPairs contNextStep)

toContNextStep
    :: ChainId
    -> P.PactExec
    -> Maybe ContinuationNextStep
toContNextStep currChainId pe
  | isLastStep = Nothing
  | otherwise = case (P._peYield pe >>= P._yProvenance) of
      -- next step occurs in the same chain
      Nothing -> Just $ ContinuationNextStep $ chainIdToText currChainId
      -- next step is a cross-chain step
      Just (P.Provenance nextChainId _) ->
        Just $ ContinuationNextStep (P._chainId nextChainId)
  where
    isLastStep = (succ $ P._peStep pe) == (P._peStepCount pe)


data ContinuationMetaData = ContinuationMetaData
  { _continuationMetaData_currStep :: !ContinuationCurrStep
  , _continuationStep_nextStep :: !(Maybe ContinuationNextStep)
  , _continuationMetaData_pactIdReqKey :: !P.PactId
  , _continuationMetaData_totalSteps :: !Int
  } deriving Show
-- TODO: document
instance ToObject ContinuationMetaData where
  toPairs (ContinuationMetaData curr next rk total) =
    [ "current-step" .= toObject curr
    , "first-step-request-key" .= rk
    , "total-steps" .= total ]
    <> omitNextIfMissing
    where
      omitNextIfMissing = case next of
        Nothing -> []
        Just ns -> [ "next-step" .= toObject ns ]
  toObject contMeta = HM.fromList (toPairs contMeta)

toContMeta :: ChainId -> P.PactExec -> ContinuationMetaData
toContMeta cid pe = ContinuationMetaData
  { _continuationMetaData_currStep = toContStep cid pe
  , _continuationStep_nextStep = toContNextStep cid pe
  , _continuationMetaData_pactIdReqKey = P._pePactId pe
  , _continuationMetaData_totalSteps = P._peStepCount pe
  }


newtype TransactionMetaData = TransactionMetaData
  { _transactionMetaData_multiStepTx :: Maybe ContinuationMetaData
  }
instance ToObject TransactionMetaData where
  toPairs (TransactionMetaData Nothing) = []
  toPairs (TransactionMetaData (Just multi)) =
    [ "multi-step-transaction" .= toObject multi ]
  toObject txMeta = HM.fromList (toPairs txMeta)

--------------------------------------------------------------------------------
-- Rosetta Helper Types --
--------------------------------------------------------------------------------

type CoinbaseTx chainwebTx = chainwebTx
newtype BalanceDelta = BalanceDelta { _balanceDelta :: Decimal }
  deriving (Show, Eq)
data AccountLog = AccountLog
  { _accountLogKey :: !T.Text
  , _accountLogBalanceTotal :: !Decimal
  , _accountLogBalanceDelta :: !BalanceDelta
  , _accountLogCurrGuard :: !Value
  , _accountLogPrevGuard :: !Value
  }
  deriving (Show, Eq)
type AccountRow = (T.Text, Decimal, Value)

type UnindexedOperation = Word64 -> [OperationId] -> Operation

data UnindexedOperations = UnindexedOperations
  { _unindexedOperation_fundOps :: [UnindexedOperation]
  , _unindexedOperation_transferOps :: [UnindexedOperation]
  , _unindexedOperation_gasOps :: [UnindexedOperation]
  }

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

rosettaTransaction :: CommandResult a -> ChainId -> [Operation] -> Transaction
rosettaTransaction cr cid ops =
  Transaction
    { _transaction_transactionId = rkToTransactionId (_crReqKey cr)
    , _transaction_operations = ops
    , _transaction_metadata = Just $ toObject (transactionMetaData cid cr)
    }

transactionMetaData :: ChainId -> CommandResult a -> TransactionMetaData
transactionMetaData cid cr = case (_crContinuation cr) of
  Nothing -> TransactionMetaData Nothing
  Just pe -> TransactionMetaData $ Just (toContMeta cid pe)

pactHashToTransactionId :: PactHash -> TransactionId
pactHashToTransactionId hsh = TransactionId $ hashToText $ toUntypedHash hsh

rkToTransactionId :: RequestKey -> TransactionId
rkToTransactionId rk = TransactionId $ requestKeyToB16Text rk

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

indexedOperations :: UnindexedOperations -> [Operation]
indexedOperations unIdxOps = fundOps <> transferOps <> gasOps
  where
    indexOps ops begIdx relatedOps =
      let related = map _operation_operationId relatedOps
          ops' = zipWith (\f i -> f i related) ops [begIdx..]
      in weaveRelatedOperations $! ops'

    fundUnIdxOps = _unindexedOperation_fundOps $! unIdxOps
    fundOps = indexOps fundUnIdxOps 0 []

    transferIdx = fromIntegral $! length fundOps
    transferUnIdxOps = _unindexedOperation_transferOps $! unIdxOps
    transferOps = indexOps transferUnIdxOps transferIdx []

    gasIdx = transferIdx + (fromIntegral $! length transferOps)
    gasUnIdxOps = _unindexedOperation_gasOps $! unIdxOps
    gasOps = indexOps gasUnIdxOps gasIdx fundOps
    -- ^ connect fund operations to gas operations

-- | Add all previous operation seen to current operation's relation operations
weaveRelatedOperations :: [Operation] -> [Operation]
weaveRelatedOperations ops = map weave opsWithRelatedOpIds
  where
    opIds = map _operation_operationId ops
    opsWithRelatedOpIds = zip ops $! inits $! opIds

    weave (op, newRelated) =
      let someOldRelated = _operation_relatedOperations op
          related = addToMaybeList newRelated someOldRelated
          sortedRelated = fmap (sortOn _operationId_index) related
      in op { _operation_relatedOperations = sortedRelated }

operation
    :: ChainwebOperationStatus
    -> OperationType
    -> P.TxId
    -> AccountLog
    -> Word64
    -> [OperationId]
    -> Operation
operation ostatus otype txId acctLog idx related =
  Operation
    { _operation_operationId = OperationId idx Nothing
    , _operation_relatedOperations = listToMaybe related
    , _operation_type = sshow otype
    , _operation_status = sshow ostatus
    , _operation_account = Just accountId
    , _operation_amount = Just $ kdaToRosettaAmount $
                          _balanceDelta $ _accountLogBalanceDelta acctLog
    , _operation_coinChange = Nothing
    , _operation_metadata = opMeta
    }
  where
    opMeta = Just $ toObject $ OperationMetaData
      {  _operationMetaData_txId = txId
      , _operationMetaData_totalBalance =
          kdaToRosettaAmount $ _accountLogBalanceTotal acctLog
      , _operationMetaData_prevOwnership = _accountLogPrevGuard acctLog
      }
    accountId = AccountId
      { _accountId_address = _accountLogKey acctLog
      , _accountId_subAccount = Nothing  -- assumes coin acct contract only
      , _accountId_metadata = accountIdMeta
      }
    accountIdMeta = Just $ toObject $ AccountIdMetaData
      { _accountIdMetaData_currOwnership = _accountLogCurrGuard acctLog
      }


-- | Timestamp of the block in milliseconds since the Unix Epoch.
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

-- | Guarantees that the `ChainId` given actually belongs to this
-- `ChainwebVersion`. This doesn't guarantee that the chain is active.
--
readChainIdText :: ChainwebVersion -> T.Text -> Maybe ChainId
readChainIdText v c = do
  cid <- readMaybe @Word (T.unpack c)
  mkChainId v maxBound cid

-- TODO: document
maxRosettaNodePeerLimit :: Natural
maxRosettaNodePeerLimit = 64

rowDataToAccountLog :: AccountRow -> Maybe AccountRow -> AccountLog
rowDataToAccountLog (currKey, currBal, currGuard) prev = do
  case prev of
    Nothing ->
      -- First time seeing account
      AccountLog
        { _accountLogKey = currKey
        , _accountLogBalanceTotal = currBal
        , _accountLogBalanceDelta = BalanceDelta currBal
        , _accountLogCurrGuard = currGuard
        , _accountLogPrevGuard = currGuard
        }
    Just (_, prevBal, prevGuard) ->
      -- Already seen this account
      AccountLog
        { _accountLogKey = currKey
        , _accountLogBalanceTotal = currBal
        , _accountLogBalanceDelta = BalanceDelta (currBal - prevBal)
        , _accountLogCurrGuard = currGuard
        , _accountLogPrevGuard = prevGuard
        }

-- | Parse TxLog Value into fungible asset account columns
txLogToAccountRow :: P.TxLog Value -> Maybe AccountRow
txLogToAccountRow (P.TxLog _ key (Object row)) = do
  guard :: Value <- (HM.lookup "guard" row) >>= (hushResult . fromJSON)
  (PLiteral (LDecimal bal)) <- (HM.lookup "balance" row) >>= (hushResult . fromJSON)
  pure $! (key, bal, guard)
txLogToAccountRow _ = Nothing

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

listToMaybe :: [a] -> Maybe [a]
listToMaybe [] = Nothing
listToMaybe li = Just li

addToMaybeList :: [a] -> Maybe [a] -> Maybe [a]
addToMaybeList newList someList = case newList of
  [] -> someList
  li -> case someList of
    Nothing -> Just li
    Just oldList -> Just $! oldList <> li
