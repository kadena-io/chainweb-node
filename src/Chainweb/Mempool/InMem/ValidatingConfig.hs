{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Chainweb.Mempool.InMem.ValidatingConfig
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.Mempool.InMem.ValidatingConfig
( validatingMempoolConfig
) where

import Control.Lens
import Chainweb.ChainId
import Chainweb.Mempool.InMemTypes
import Chainweb.Mempool.Mempool
import Chainweb.Pact.Transaction qualified as Pact
import Chainweb.Pact.Validations
import Chainweb.Utils
import Chainweb.Version
import Data.These
import Data.Vector qualified as V
import qualified Pact.Core.Command.Types as Pact
import qualified Pact.Core.ChainData as Pact

validatingMempoolConfig
    :: ChainId
    -> ChainwebVersion
    -> GasLimit
    -> GasPrice
    -> (V.Vector Pact.Transaction -> IO (V.Vector (Maybe InsertError)))
    -> InMemConfig Pact.Transaction
validatingMempoolConfig cid v gl gp preInsertCheck = InMemConfig
    { _inmemTxCfg = pactTransactionConfig
    , _inmemTxBlockSizeLimit = gl
    , _inmemTxMinGasPrice = gp
    , _inmemMaxRecentItems = maxRecentLog
    , _inmemPreInsertPureChecks = preInsertSingle
    , _inmemPreInsertBatchChecks = preInsertBatch
    , _inmemCurrentTxsSize = currentTxsSize
    }
  where
    maxRecentLog = 2048

    currentTxsSize = 1024 * 1024 -- ~16MB per mempool
        -- 1M items is is sufficient for supporing about 12 TPS per chain, which
        -- is about 360 tx per block. Larger TPS values would result in false
        -- negatives in the set.

    -- | Validation: Is this TX associated with the correct `ChainId`?
    --
    preInsertSingle :: Pact.Transaction -> Either InsertError Pact.Transaction
    preInsertSingle tx = do
        let !pay = view Pact.payloadObj . Pact._cmdPayload $ tx
            pcid = Pact._pmChainId $ Pact._pMeta pay
            sigs = Pact._cmdSigs tx
            ver  = Pact._pNetworkId pay
        if | not $ assertChainId cid pcid  -> Left InsertErrorMetadataMismatch
           | not $ assertSigSize sigs      -> Left InsertErrorTooManySigs
           | not $ assertNetworkId v ver   -> Left InsertErrorMetadataMismatch
           | otherwise                     -> Right tx

    -- | Validation: All checks that should occur before a TX is inserted into
    -- the mempool. A rejection at this stage means that something is
    -- fundamentally wrong/illegal with the TX, and that it should be rejected
    -- completely and not gossiped to other peers.
    --
    -- We expect this to be called in two places: once when a new Pact
    -- Transaction is submitted via the @send@ endpoint, and once when a new TX
    -- is gossiped to us from a peer's mempool.
    --
    preInsertBatch
        :: V.Vector (T2 TransactionHash Pact.Transaction)
        -> IO (V.Vector (Either (T2 TransactionHash InsertError)
                                (T2 TransactionHash Pact.Transaction)))
    preInsertBatch txs
        | V.null txs = return V.empty
        | otherwise = do
            rs <- preInsertCheck (V.map ssnd txs)
            pure $ alignWithV f rs txs
      where
        f (These r (T2 h t)) = case r of
                                 Just e -> Left (T2 h e)
                                 Nothing -> Right (T2 h t)
        f (That _) = alignmentError
        f (This _) = alignmentError
        alignmentError = error "internal error: mismatch between input and output length of pre-insert check"
