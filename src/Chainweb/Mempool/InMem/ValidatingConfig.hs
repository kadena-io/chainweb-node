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

import Chainweb.ChainId
import Chainweb.Mempool.InMemTypes
import Chainweb.Mempool.Mempool
import Chainweb.Pact4.Transaction qualified as Pact4
import Chainweb.Pact4.Validations
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebPactExecutionService
import Control.Concurrent
import Data.These
import Data.Vector qualified as V
import Pact.Types.ChainMeta
import Pact.Types.Command

validatingMempoolConfig
    :: ChainId
    -> ChainwebVersion
    -> GasLimit
    -> GasPrice
    -> MVar PactExecutionService
    -> InMemConfig Pact4.UnparsedTransaction
validatingMempoolConfig cid v gl gp mv = InMemConfig
    { _inmemTxCfg = txcfg
    , _inmemTxBlockSizeLimit = gl
    , _inmemTxMinGasPrice = gp
    , _inmemMaxRecentItems = maxRecentLog
    , _inmemPreInsertPureChecks = preInsertSingle
    , _inmemPreInsertBatchChecks = preInsertBatch
    , _inmemCurrentTxsSize = currentTxsSize
    }
  where
    txcfg = pact4TransactionConfig
        -- The mempool doesn't provide a chain context to the codec which means
        -- that the latest version of the parser is used.

    maxRecentLog = 2048

    currentTxsSize = 1024 * 1024 -- ~16MB per mempool
        -- 1M items is is sufficient for supporing about 12 TPS per chain, which
        -- is about 360 tx per block. Larger TPS values would result in false
        -- negatives in the set.

    -- | Validation: Is this TX associated with the correct `ChainId`?
    --
    preInsertSingle :: Pact4.UnparsedTransaction -> Either InsertError Pact4.UnparsedTransaction
    preInsertSingle tx = do
        let !pay = Pact4.payloadObj . _cmdPayload $ tx
            pcid = _pmChainId $ _pMeta pay
            sigs = _cmdSigs tx
            ver  = _pNetworkId pay
        if | not $ assertParseChainId pcid -> Left $ InsertErrorOther "Unparsable ChainId"
           | not $ assertChainId cid pcid  -> Left InsertErrorMetadataMismatch
           | not $ assertSigSize sigs      -> Left $ InsertErrorOther "Too many signatures"
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
        :: V.Vector (T2 TransactionHash Pact4.UnparsedTransaction)
        -> IO (V.Vector (Either (T2 TransactionHash InsertError)
                                (T2 TransactionHash Pact4.UnparsedTransaction)))
    preInsertBatch txs
        | V.null txs = return V.empty
        | otherwise = do
            pex <- readMVar mv
            rs <- _pactPreInsertCheck pex cid (V.map ssnd txs)
            pure $ alignWithV f rs txs
      where
        f (These r (T2 h t)) = case r of
                                 Just e -> Left (T2 h e)
                                 Nothing -> Right (T2 h t)
        f (That (T2 h _)) = Left (T2 h $ InsertErrorOther "preInsertBatch: align mismatch 0")
        f (This _) = Left (T2 (TransactionHash "") (InsertErrorOther "preInsertBatch: align mismatch 1"))


