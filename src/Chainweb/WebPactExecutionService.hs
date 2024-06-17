{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Chainweb.WebPactExecutionService
  ( WebPactExecutionService(..)
  , _webPactNewBlock
  , _webPactValidateBlock
  , _webPactSyncToBlock
  , PactExecutionService(..)
  , mkWebPactExecutionService
  , mkPactExecutionService
  , emptyPactExecutionService
  , NewBlock(..)
  , newBlockToPayloadWithOutputs
  , newBlockParentHeader
  ) where

import Control.Monad.Catch

import qualified Data.HashMap.Strict as HM
import Data.Vector (Vector)
import qualified Data.Vector as V

import GHC.Stack

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Mempool.Mempool (InsertError)
import Chainweb.Miner.Pact
import Chainweb.Pact.Service.BlockValidation
import Chainweb.Pact.Service.PactQueue
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Utils
import Chainweb.Payload
import Chainweb.Transaction
import Chainweb.Utils

import Pact.Types.Hash
import Pact.Types.Persistence (RowKey, TxLog, Domain)
import Pact.Types.RowData (RowData)

-- -------------------------------------------------------------------------- --
-- PactExecutionService

data NewBlock
  = NewBlockInProgress !BlockInProgress
  | NewBlockPayload !ParentHeader !PayloadWithOutputs

newBlockToPayloadWithOutputs :: NewBlock -> PayloadWithOutputs
newBlockToPayloadWithOutputs (NewBlockInProgress bip)
    = blockInProgressToPayloadWithOutputs bip
newBlockToPayloadWithOutputs (NewBlockPayload _ pwo)
    = pwo

newBlockParentHeader :: NewBlock -> ParentHeader
newBlockParentHeader (NewBlockInProgress bip) = _blockInProgressParentHeader bip
newBlockParentHeader (NewBlockPayload ph _) = ph

-- | Service API for interacting with a single or multi-chain ("Web") pact service.
-- Thread-safe to be called from multiple threads. Backend is queue-backed on a per-chain
-- basis.
data PactExecutionService = PactExecutionService
    { _pactValidateBlock :: !(
        BlockHeader ->
        CheckablePayload ->
        IO PayloadWithOutputs
        )
      -- ^ Validate block payload data by running through pact service.
    , _pactNewBlock :: !(
        ChainId ->
        Miner ->
        NewBlockFill ->
        IO NewBlock
        )
    , _pactContinueBlock :: !(
        ChainId ->
        BlockInProgress ->
        IO (Historical BlockInProgress)
        )
      -- ^ Request a new block to be formed using mempool
    , _pactLocal :: !(
        Maybe LocalPreflightSimulation ->
        Maybe LocalSignatureVerification ->
        Maybe RewindDepth ->
        ChainwebTransaction ->
        IO LocalResult)
      -- ^ Directly execute a single transaction in "local" mode (all DB interactions rolled back).
      -- Corresponds to `local` HTTP endpoint.
    , _pactLookup :: !(
        ChainId
        -- for routing
        -> Maybe ConfirmationDepth
        -- confirmation depth
        -> Vector PactHash
        -- txs to lookup
        -> IO (HM.HashMap PactHash (T2 BlockHeight BlockHash))
        )
    , _pactReadOnlyReplay :: !(
        BlockHeader ->
        Maybe BlockHeader ->
        IO ()
      )
      -- ^ Lookup pact hashes as of a block header to detect duplicates
    , _pactPreInsertCheck :: !(
        ChainId
        -> Vector ChainwebTransaction
        -> IO (Vector (Either InsertError ())))
      -- ^ Run speculative checks to find bad transactions (ie gas buy failures, etc)
    , _pactBlockTxHistory :: !(
        BlockHeader ->
        Domain RowKey RowData ->
        IO (Historical BlockTxHistory)
        )
      -- ^ Obtain all transaction history in block for specified table/domain.
    , _pactHistoricalLookup :: !(
        BlockHeader ->
        Domain RowKey RowData ->
        RowKey ->
        IO (Historical (Maybe (TxLog RowData)))
        )
      -- ^ Obtain latest entry at or before the given block for specified table/domain and row key.
    , _pactSyncToBlock :: !(
        BlockHeader ->
        IO ()
        )
    }

-- | Newtype to indicate "routing"/multi-chain service.
-- See 'mkWebPactExecutionService' for implementation.
newtype WebPactExecutionService = WebPactExecutionService
    { _webPactExecutionService :: PactExecutionService
    }


_webPactNewBlock
    :: WebPactExecutionService
    -> ChainId
    -> Miner
    -> NewBlockFill
    -> IO NewBlock
_webPactNewBlock = _pactNewBlock . _webPactExecutionService
{-# INLINE _webPactNewBlock #-}

_webPactContinueBlock
    :: WebPactExecutionService
    -> ChainId
    -> BlockInProgress
    -> IO (Historical BlockInProgress)
_webPactContinueBlock = _pactContinueBlock . _webPactExecutionService
{-# INLINE _webPactContinueBlock #-}

_webPactValidateBlock
    :: WebPactExecutionService
    -> BlockHeader
    -> CheckablePayload
    -> IO PayloadWithOutputs
_webPactValidateBlock = _pactValidateBlock . _webPactExecutionService
{-# INLINE _webPactValidateBlock #-}

_webPactSyncToBlock
    :: WebPactExecutionService
    -> BlockHeader
    -> IO ()
_webPactSyncToBlock = _pactSyncToBlock . _webPactExecutionService
{-# INLINE _webPactSyncToBlock #-}

mkWebPactExecutionService
    :: HasCallStack
    => HM.HashMap ChainId PactExecutionService
    -> WebPactExecutionService
mkWebPactExecutionService hm = WebPactExecutionService $ PactExecutionService
    { _pactValidateBlock = \h pd -> withChainService (_chainId h) $ \p -> _pactValidateBlock p h pd
    , _pactNewBlock = \cid m fill -> withChainService cid $ \p -> _pactNewBlock p cid m fill
    , _pactContinueBlock = \cid bip -> withChainService cid $ \p -> _pactContinueBlock p cid bip
    , _pactLocal = \_pf _sv _rd _ct -> throwM $ userError "Chainweb.WebPactExecutionService.mkPactExecutionService: No web-level local execution supported"
    , _pactLookup = \cid cd txs -> withChainService cid $ \p -> _pactLookup p cid cd txs
    , _pactPreInsertCheck = \cid txs -> withChainService cid $ \p -> _pactPreInsertCheck p cid txs
    , _pactBlockTxHistory = \h d -> withChainService (_chainId h) $ \p -> _pactBlockTxHistory p h d
    , _pactHistoricalLookup = \h d k -> withChainService (_chainId h) $ \p -> _pactHistoricalLookup p h d k
    , _pactSyncToBlock = \h -> withChainService (_chainId h) $ \p -> _pactSyncToBlock p h
    , _pactReadOnlyReplay = \l u -> withChainService (_chainId l) $ \p -> _pactReadOnlyReplay p l u
    }
  where
    withChainService cid act =  maybe (err cid) act $ HM.lookup cid hm
    err cid = throwM $ userError
      $ "PactExecutionService: Invalid chain ID: "
      ++ show cid

mkPactExecutionService
    :: PactQueue
    -> PactExecutionService
mkPactExecutionService q = PactExecutionService
    { _pactValidateBlock = \h pd -> do
        validateBlock h pd q
    , _pactNewBlock = \_ m fill -> do
        NewBlockInProgress <$> newBlock m fill q
    , _pactContinueBlock = \_ bip -> do
        continueBlock bip q
    , _pactLocal = \pf sv rd ct ->
        local pf sv rd ct q
    , _pactLookup = \_ cd txs ->
        lookupPactTxs cd txs q
    , _pactPreInsertCheck = \_ txs ->
        pactPreInsertCheck txs q
    , _pactBlockTxHistory = \h d ->
        pactBlockTxHistory h d q
    , _pactHistoricalLookup = \h d k ->
        pactHistoricalLookup h d k q
    , _pactSyncToBlock = \h -> pactSyncToBlock h q
   , _pactReadOnlyReplay = \l u -> pactReadOnlyReplay l u q
    }

-- | A mock execution service for testing scenarios. Throws out anything it's
-- given.
--
emptyPactExecutionService :: HasCallStack => PactExecutionService
emptyPactExecutionService = PactExecutionService
    { _pactValidateBlock = \_ _ -> pure emptyPayload
    , _pactNewBlock = \_ _ _ -> throwM (userError "emptyPactExecutionService: attempted `newBlock` call")
    , _pactContinueBlock = \_ _ -> throwM (userError "emptyPactExecutionService: attempted `continueBlock` call")
    , _pactLocal = \_ _ _ _ -> throwM (userError "emptyPactExecutionService: attempted `local` call")
    , _pactLookup = \_ _ _ -> return $! HM.empty
    , _pactPreInsertCheck = \_ txs -> return $ V.map (const (Right ())) txs
    , _pactBlockTxHistory = \_ _ -> error "Chainweb.WebPactExecutionService.emptyPactExecutionService: pactBlockTxHistory unsupported"
    , _pactHistoricalLookup = \_ _ _ -> error "Chainweb.WebPactExecutionService.emptyPactExecutionService: pactHistoryLookup unsupported"
    , _pactSyncToBlock = \_ -> return ()
    , _pactReadOnlyReplay = \_ _ -> return ()
    }
