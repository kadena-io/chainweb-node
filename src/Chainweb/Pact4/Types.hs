{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Chainweb.Pact4.Types
  ( GasSupply(..)

    -- * TxContext
  -- , TxContext(..)
  -- , tcParentHeader
  -- , tcPublicMeta
  -- , tcMiner
  -- , tcIsGenesis
  -- , ctxToPublicData
  -- , ctxToPublicData'
  -- , ctxBlockHeader
  -- , ctxCurrentBlockHeight
  -- , ctxParentBlockHeight
  -- , ctxChainId
  -- , guardCtx
  -- , getTxContext

  , catchesPactError
  , UnexpectedErrorPrinting(..)
  , GasId(..)
  , EnforceCoinbaseFailure(..)
  , CoinbaseUsePrecompiled(..)
  , internalError
  , PactInternalError(..)
  , SQLiteRowDelta(..)
  , SQLitePendingData(..)
  , emptySQLitePendingData
  , pendingTableCreation
  , pendingWrites
  , pendingTxLogMap
  , pendingSuccessfulTxs
  , toPayloadWithOutputs
  , TxTimeout(..)

  , getGasModel
  ) where

import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.Pact.Payload(PayloadWithOutputs, newBlockOutputs, blockPayload, payloadData, payloadWithOutputs, newBlockTransactions, Transaction (..), TransactionOutput (..), CoinbaseOutput (..))
import Chainweb.Pact.Types (Transactions(..), BlockCtx, guardCtx)
import Chainweb.Pact4.Transaction qualified as Pact
import Chainweb.Utils
import Chainweb.Version
import Chainweb.Version.Guards
import Control.Exception.Safe
import Control.Lens
import Control.Monad.Reader
import Data.Aeson hiding (Error,(.=))
import Data.ByteString (ByteString)
import Data.ByteString.Short qualified as SB
import Data.DList (DList)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Generics
import GHC.Stack (CallStack, HasCallStack, callStack)
import Pact.Gas.Table
import Pact.JSON.Encode qualified as J
import Pact.Parse (ParsedDecimal)
import Pact.Types.Command
import Pact.Types.Gas
import Pact.Types.Info
import Pact.Types.Persistence qualified as Pact
import Pact.Types.Pretty (viaShow)
import Pact.Types.Runtime (PactError(..), PactErrorType(..))
import Pact.Types.Runtime qualified as Pact
import Pact.Types.Term
import System.LogLevel


-- | Indicates a computed gas charge (gas amount * gas price)
newtype GasSupply = GasSupply { _gasSupply :: ParsedDecimal }
   deriving (Eq,Ord)
   deriving newtype (Num,Real,Fractional,FromJSON)
instance Show GasSupply where show (GasSupply g) = show g

instance J.Encode GasSupply where
    build = J.build . _gasSupply

-- -- | Pair parent header with transaction metadata.
-- -- In cases where there is no transaction/Command, 'PublicMeta'
-- -- default value is used.
-- data TxContext = TxContext
--   { _tcParentHeader :: !(Parent BlockHeader)
--   , _tcPublicMeta :: !PublicMeta
--   , _tcMiner :: !Miner
--   , _tcIsGenesis :: !Bool
--   } deriving Show

-- makeLenses ''TxContext

-- instance HasChainId TxContext where
--   _chainId = _chainId . _tcParentHeader

-- -- | Retrieve parent header as 'BlockHeader'
-- ctxBlockHeader :: TxContext -> BlockHeader
-- ctxBlockHeader = view _Parent . _tcParentHeader

-- -- | Get "current" block height, which means parent height + 1.
-- -- This reflects Pact environment focus on current block height,
-- -- which influenced legacy switch checks as well.
-- ctxCurrentBlockHeight :: TxContext -> BlockHeight
-- ctxCurrentBlockHeight = succ . view blockHeight . ctxBlockHeader

-- -- | Get "current" block height, which means parent height + 1.
-- -- This reflects Pact environment focus on current block height,
-- -- which influenced legacy switch checks as well.
-- ctxParentBlockHeight :: TxContext -> Parent BlockHeight
-- ctxParentBlockHeight = Parent . view blockHeight . ctxBlockHeader

-- ctxChainId :: TxContext -> ChainId
-- ctxChainId = view blockChainId . ctxBlockHeader

-- guardCtx :: (ChainId -> BlockHeight -> a) -> TxContext -> a
-- guardCtx g txCtx = g (ctxChainId txCtx) (ctxCurrentBlockHeight txCtx)

-- | Assemble tx context from transaction metadata and parent header.
-- getTxContext :: Miner -> PublicMeta -> Parent BlockHeader -> TxContext
-- getTxContext miner pm ph = TxContext ph pm miner

data UnexpectedErrorPrinting = PrintsUnexpectedError | CensorsUnexpectedError

catchesPactError :: (MonadCatch m, MonadIO m, Logger logger) => logger -> UnexpectedErrorPrinting -> m a -> m (Either PactError a)
catchesPactError logger exnPrinting action = catches (Right <$> action)
  [ Handler $ \(e :: PactError) -> return $ Left e
  , Handler $ \(e :: SomeException) -> do
      !err <- case exnPrinting of
          PrintsUnexpectedError ->
            return (viaShow e)
          CensorsUnexpectedError -> do
            liftIO $ logFunctionText logger Warn ("catchesPactError: unknown error: " <> sshow e)
            return "unknown error"
      return $ Left $ PactError EvalError noInfo [] err
  ]


newtype GasId = GasId PactId deriving (Eq, Show)

-- | Whether to enforce coinbase failures, failing the block,
-- or be backward-compatible and allow.
-- Backward-compat fix is to enforce in new block, but ignore in validate.
--
newtype EnforceCoinbaseFailure = EnforceCoinbaseFailure Bool

-- | Always use precompiled templates in coinbase or use date rule.
newtype CoinbaseUsePrecompiled = CoinbaseUsePrecompiled Bool

-- | Modified table gas module with free module loads
--
freeModuleLoadGasModel :: GasModel
freeModuleLoadGasModel = modifiedGasModel
  where
    defGasModel = tableGasModel defaultGasConfig
    fullRunFunction = runGasModel defGasModel
    modifiedRunFunction name ga = case ga of
      GPostRead ReadModule {} -> MilliGas 0
      _ -> fullRunFunction name ga
    modifiedGasModel = defGasModel { runGasModel = modifiedRunFunction }

chainweb213GasModel :: GasModel
chainweb213GasModel = modifiedGasModel
  where
    defGasModel = tableGasModel gasConfig
    unknownOperationPenalty = 1000000
    multiRowOperation = 40000
    gasConfig = defaultGasConfig { _gasCostConfig_primTable = updTable }
    updTable = M.union upd defaultGasTable
    upd = M.fromList
      [("keys",    multiRowOperation)
      ,("select",  multiRowOperation)
      ,("fold-db", multiRowOperation)
      ]
    fullRunFunction = runGasModel defGasModel
    modifiedRunFunction name ga = case ga of
      GPostRead ReadModule {} -> 0
      GUnreduced _ts -> case M.lookup name updTable of
        Just g -> g
        Nothing -> unknownOperationPenalty
      _ -> milliGasToGas $ fullRunFunction name ga
    modifiedGasModel = defGasModel { runGasModel = \t g -> gasToMilliGas (modifiedRunFunction t g) }

chainweb224GasModel :: GasModel
chainweb224GasModel = chainweb213GasModel
  { runGasModel = \name -> \case
    GPostRead ReadInterface {} -> MilliGas 0
    ga -> runGasModel chainweb213GasModel name ga
  }

getGasModel :: HasVersion => BlockCtx -> GasModel
getGasModel ctx
    | guardCtx chainweb213Pact ctx = chainweb213GasModel
    | guardCtx chainweb224Pact ctx = chainweb224GasModel
    | otherwise = freeModuleLoadGasModel

-- | While a block is being run, mutations to the pact database are held
-- in RAM to be written to the DB in batches at @save@ time. For any given db
-- write, we need to record the table name, the current tx id, the row key, and
-- the row value.
--
data SQLiteRowDelta = SQLiteRowDelta
    { _deltaTableName :: !Text
    , _deltaTxId :: {-# UNPACK #-} !Pact.TxId
    , _deltaRowKey :: !ByteString
    , _deltaData :: !ByteString
    } deriving (Show, Generic, Eq)

instance Ord SQLiteRowDelta where
    compare a b = compare aa bb
        where
        aa = (_deltaTableName a, _deltaRowKey a, _deltaTxId a)
        bb = (_deltaTableName b, _deltaRowKey b, _deltaTxId b)
    {-# INLINE compare #-}

-- | A map from table name to a list of 'TxLog' entries. This is maintained in
-- 'BlockState' and is cleared upon pact transaction commit.
type TxLogMap = M.Map Pact.TableName (DList Pact.TxLogJson)

-- | Between a @restore..save@ bracket, we also need to record which tables
-- were created during this block (so the necessary @CREATE TABLE@ statements
-- can be performed upon block save).
type SQLitePendingTableCreations = HashSet Text

-- | Pact transaction hashes resolved during this block.
type SQLitePendingSuccessfulTxs = HashSet ByteString

-- | Pending writes to the pact db during a block, to be recorded in 'BlockState'.
-- Structured as a map from table name to a map from rowkey to inserted row delta.
type SQLitePendingWrites = HashMap Text (HashMap ByteString (NonEmpty SQLiteRowDelta))

-- Note [TxLogs in SQLitePendingData]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We should really not store TxLogs in SQLitePendingData,
-- because this data structure is specifically for things that
-- can exist both for the whole block and for specific transactions,
-- and txlogs only exist on the transaction level.
-- We don't do this in Pact 5 at all.

-- | A collection of pending mutations to the pact db. We maintain two of
-- these; one for the block as a whole, and one for any pending pact
-- transaction. Upon pact transaction commit, the two 'SQLitePendingData'
-- values are merged together.
data SQLitePendingData = SQLitePendingData
    { _pendingTableCreation :: !SQLitePendingTableCreations
    , _pendingWrites :: !SQLitePendingWrites
    -- See Note [TxLogs in SQLitePendingData]
    , _pendingTxLogMap :: !TxLogMap
    , _pendingSuccessfulTxs :: !SQLitePendingSuccessfulTxs
    }
    deriving (Eq, Show)

emptySQLitePendingData :: SQLitePendingData
emptySQLitePendingData = SQLitePendingData mempty mempty mempty mempty

data PactInternalError
  = PactInternalError !CallStack !Text
  | BlockValidationFailure !Text
  | PactDuplicateTableError !Text
  | PactTransactionExecError !Pact.PactHash !Text
  | PactTransactionValidationException !Text
  | CoinbaseFailure !Text
  | PactBuyGasFailure !Text
  | BlockGasLimitExceeded !Pact.Gas
  | TransactionDecodeFailure !Text
  deriving stock (Generic)
  deriving anyclass (Exception)

instance Show PactInternalError where
    show = T.unpack . J.encodeText

internalError :: (HasCallStack, MonadThrow m) => Text -> m a
internalError = throwM . PactInternalError callStack

instance J.Encode PactInternalError where
  build (PactInternalError _stack msg) = tagged "PactInternalError" msg
  build (PactDuplicateTableError msg) = tagged "PactDuplicateTableError" msg
  build (PactTransactionExecError h msg) = tagged "PactTransactionExecError" (J.Array (h, msg))
  build (PactTransactionValidationException errs) = tagged "PactTransactionValidationException" errs
  build (CoinbaseFailure msg) = tagged "CoinbaseFailure" msg
  build (PactBuyGasFailure msg) = tagged "PactBuyGasFailure" msg
  build (BlockValidationFailure msg) = tagged "BlockValidationFailure" msg
  build (BlockGasLimitExceeded g) = tagged "BlockGasLimitExceeded" g
  build (TransactionDecodeFailure g) = tagged "TransactionDecodeFailure" g

tagged :: J.Encode v => Text -> v -> J.Builder
tagged t v = J.object
    [ "tag" J..= t
    , "contents" J..= v
    ]

makeLenses ''SQLitePendingData

pactCommandToBytes :: Command Text -> Transaction
pactCommandToBytes cwTrans =
    let plBytes = J.encodeStrict cwTrans
    in Transaction { _transactionBytes = plBytes }

pactCommandResultToBytes :: CommandResult Pact.Hash -> TransactionOutput
pactCommandResultToBytes cr =
    let outBytes = J.encodeStrict cr
    in TransactionOutput { _transactionOutputBytes = outBytes }

hashPactTxLogs :: CommandResult [Pact.TxLogJson] -> CommandResult Pact.Hash
hashPactTxLogs = over (crLogs . _Just) $ Pact.pactHash . Pact.encodeTxLogJsonArray

toPayloadWithOutputs :: Miner -> Transactions Pact.Transaction (CommandResult [Pact.TxLogJson]) -> PayloadWithOutputs
toPayloadWithOutputs mi ts =
    let oldSeq = _transactionPairs ts
        trans = cmdBSToTx . sfst <$> oldSeq
        transOuts = pactCommandResultToBytes . hashPactTxLogs . ssnd <$> oldSeq

        miner = toMinerData mi
        cb = CoinbaseOutput $ J.encodeStrict $ hashPactTxLogs $ _transactionCoinbase ts
        blockTrans = snd $ newBlockTransactions miner trans
        cmdBSToTx = pactCommandToBytes
          . fmap (T.decodeUtf8 . SB.fromShort . Pact.payloadBytes)
        blockOuts = snd $ newBlockOutputs cb transOuts

        blockPL = blockPayload blockTrans blockOuts
        plData = payloadData blockTrans blockPL
    in payloadWithOutputs plData cb transOuts

newtype TxTimeout = TxTimeout Text
    deriving Show
instance Exception TxTimeout
