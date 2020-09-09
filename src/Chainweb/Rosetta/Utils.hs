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

-- | Helper typeclass for transforming Rosetta metadata into a
--   JSON Object.
-- NOTE: Rosetta types expect metadata to be `Object`
class ToObject a where
  toPairs :: a -> [(T.Text, Value)]
  toObject :: a -> Object


data OperationMetaData = OperationMetaData
  { _operationMetaData_txId :: !P.TxId
  , _operationMetaData_totalBalance :: !Amount
  , _operationMetaData_prevOwnership :: !Value
  , _operationMetaData_currOwnership :: !Value --TODO: hack for rotation bug
  } deriving Show
-- TODO: document
instance ToObject OperationMetaData where
  toPairs (OperationMetaData txId bal prevOwnership currOwnership) =
    [ "tx-id" .= txId
    , "total-balance" .= bal
    , "prev-ownership" .= prevOwnership
    , "curr-ownership" .= currOwnership ]
  toObject opMeta = HM.fromList (toPairs opMeta)


newtype AccountIdMetaData = AccountIdMetaData
  { _accountIdMetaData_currOwnership :: Value }
  deriving Show
-- TODO: document
instance ToObject AccountIdMetaData where
  toPairs (AccountIdMetaData currOwnership) =
    [ "current-ownership" .= currOwnership ]
  toObject acctMeta = HM.fromList (toPairs acctMeta)


newtype TransactionMetaData = TransactionMetaData
  { _transactionMetaData_multiStepTx :: Maybe ContinuationMetaData
  }
instance ToObject TransactionMetaData where
  toPairs (TransactionMetaData Nothing) = []
  toPairs (TransactionMetaData (Just multi)) =
    [ "multi-step-transaction" .= toObject multi ]
  toObject txMeta = HM.fromList (toPairs txMeta)

transactionMetaData :: ChainId -> CommandResult a -> TransactionMetaData
transactionMetaData cid cr = case (_crContinuation cr) of
  Nothing -> TransactionMetaData Nothing
  Just pe -> TransactionMetaData $ Just (toContMeta cid pe)


-- | Adds more transparency into continuation transactions.
--
data ContinuationMetaData = ContinuationMetaData
  { _continuationMetaData_currStep :: !ContinuationCurrStep
  -- ^ Information on the current step in the continuation.
  , _continuationStep_nextStep :: !(Maybe ContinuationNextStep)
  -- ^ Information on the next step in the continuation (if there is one).
  , _continuationMetaData_pactIdReqKey :: !P.PactId
  -- ^ The request key of the transaction that initiated this continuation.
  -- TODO: Further work needs to be done to know WHICH chain this
  --       initial transaction occurred in.
  , _continuationMetaData_totalSteps :: !Int
  -- ^ Total number of steps in the entire continuation.
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


-- | Provides information on the step that was just executed.
data ContinuationCurrStep = ContinuationCurrStep
  { _continuationCurrStep_chainId :: !T.Text
  -- ^ Chain id where step was executed
  , _continuationCurrStep_stepId :: !Int
  -- ^ Step that was executed or skipped
  , _continuationCurrStep_rollbackAvailable :: Bool
  -- ^ Track whether a current step allows for rollbacks
  } deriving Show
-- TODO: Add ability to detect if step was rolled back.
-- TODO: document
instance ToObject ContinuationCurrStep where
  toPairs (ContinuationCurrStep cid step rollback) =
    [ "chain-id" .= cid
    , "step-id" .= step
    , "rollback-available" .= rollback ]
  toObject contCurrStep = HM.fromList (toPairs contCurrStep)

toContStep :: ChainId -> P.PactExec -> ContinuationCurrStep
toContStep cid pe = ContinuationCurrStep
  { _continuationCurrStep_chainId = chainIdToText cid
  , _continuationCurrStep_stepId = P._peStep pe
  , _continuationCurrStep_rollbackAvailable = P._peStepHasRollback pe
  }


-- | Indicates if the next step of a continuation occurs in a
--   different chain or in the same chain.
newtype ContinuationNextStep = ContinuationNextStep
  { _continuationNextStep_chainId :: T.Text
  } deriving Show
-- TODO: document
instance ToObject ContinuationNextStep where
  toPairs (ContinuationNextStep cid) = [ "target-chain-id" .= cid ]
  toObject contNextStep = HM.fromList (toPairs contNextStep)

-- | Determines if the continuation has a next step and, if so, provides
--   the chain id of where this next step will need to occur.
toContNextStep
    :: ChainId
    -> P.PactExec
    -> Maybe ContinuationNextStep
toContNextStep currChainId pe
  | isLastStep = Nothing
  -- TODO: Add check to see if curr step was rolled back.
  --       This would also mean a next step is not occuring.
  | otherwise = case (P._peYield pe >>= P._yProvenance) of
      Nothing -> Just $ ContinuationNextStep $ chainIdToText currChainId
      -- ^ next step occurs in the same chain
      Just (P.Provenance nextChainId _) ->
      -- ^ next step is a cross-chain step
        Just $ ContinuationNextStep (P._chainId nextChainId)
  where
    isLastStep = (succ $ P._peStep pe) == (P._peStepCount pe)

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

-- | An operation index and related operations can only be
--   determined once all operations in a transaction are known.
type UnindexedOperation =
     Word64
  -- ^ Operation index
  -> [OperationId]
  -- ^ Id of Related Operations
  -> Operation

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

-- | Flatten operations grouped by TxId into a single list of operations;
--   give each operation a unique, numerical operation id based on its position
--   in this new flattened list; and create DAG of related operations.
indexedOperations :: UnindexedOperations -> [Operation]
indexedOperations unIdxOps = fundOps <> transferOps <> gasOps
  where
    opIds = map _operation_operationId

    createOps opsF begIdx defRelatedOpIds =
      let ops = zipWith (\f i -> f i defRelatedOpIds) opsF [begIdx..]
      in weaveRelatedOperations $! ops
      -- connect operations to each other

    fundUnIdxOps = _unindexedOperation_fundOps $! unIdxOps
    fundOps = createOps fundUnIdxOps 0 []

    transferIdx = fromIntegral $! length fundOps
    transferUnIdxOps = _unindexedOperation_transferOps $! unIdxOps
    transferOps = createOps transferUnIdxOps transferIdx []

    gasIdx = transferIdx + (fromIntegral $! length transferOps)
    gasUnIdxOps = _unindexedOperation_gasOps $! unIdxOps
    gasOps = createOps gasUnIdxOps gasIdx (opIds $! fundOps)
    -- connect gas operations to fund operations

-- | Create a DAG of related operations.
-- Algorithm:
--   Given a list of operations that are related:
--     For operation x at position i,
--       Overwrite or append all operations ids at
--         position 0th to ith (not inclusive) to operation x's
--         related operations list.
-- Example: list of operations to weave: [ 4: [], 5: [1], 6: [] ]
--          weaved operations: [ 4: [], 5: [1, 4], 6: [4, 5] ]
weaveRelatedOperations :: [Operation] -> [Operation]
weaveRelatedOperations relatedOps = map weave opsWithRelatedOpIds
  where
    -- example: [1, 2, 3] -> [[], [1], [1,2], [1,2,3]]
    opIdsDAG = inits $! map _operation_operationId relatedOps
    -- example: [(op 1, []), (op 2, [1]), (op 3, [1,2])]
    opsWithRelatedOpIds = zip relatedOps opIdsDAG

    -- related operation ids must be in descending order.
    justSortRelated r = Just $! sortOn _operationId_index r

    weave (op, newRelatedIds) =
      case newRelatedIds of
        [] -> op  -- no new related operations to add
        l -> case (_operation_relatedOperations op) of
          Nothing -> op  -- no previous related operations
            { _operation_relatedOperations = justSortRelated l }
          Just oldRelatedIds -> op
            { _operation_relatedOperations = justSortRelated $! (oldRelatedIds <> l) }

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
    , _operation_relatedOperations = someRelatedOps
    , _operation_type = sshow otype
    , _operation_status = sshow ostatus
    , _operation_account = Just accountId
    , _operation_amount = Just $ kdaToRosettaAmount $
                          _balanceDelta $ _accountLogBalanceDelta acctLog
    , _operation_coinChange = Nothing
    , _operation_metadata = opMeta
    }
  where
    someRelatedOps = case related of
      [] -> Nothing
      li -> Just li
    opMeta = Just $ toObject $ OperationMetaData
      { _operationMetaData_txId = txId
      , _operationMetaData_totalBalance =
          kdaToRosettaAmount $ _accountLogBalanceTotal acctLog
      , _operationMetaData_prevOwnership = _accountLogPrevGuard acctLog
      , _operationMetaData_currOwnership = _accountLogCurrGuard acctLog
      }
    accountId = AccountId
      { _accountId_address = _accountLogKey acctLog
      , _accountId_subAccount = Nothing  -- assumes coin acct contract only
      , _accountId_metadata = Nothing -- disabled due to ownership rotation bug
      }
    _accountIdMeta = Just $ toObject $ AccountIdMetaData
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
