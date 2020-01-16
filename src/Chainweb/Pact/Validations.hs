{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module: Chainweb.Pact.
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- Pact service and mempool data validations
--
module Chainweb.Pact.Validations
( assertChainId
, assertGasPrice
, assertNetworkId
, assertBlockTime
, assertSigSize
, assertTxSize
, assertCompile
) where


import Data.Decimal

import Pact.Compile
import Pact.Parse
import qualified Pact.Types.ChainId as P
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Gas
import Pact.Types.RPC
import Pact.Types.Term

import Chainweb.BlockHeader
import Chainweb.Mempool.Mempool
import Chainweb.Pact.Utils
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Utils
import Chainweb.Version hiding (ChainId)
import qualified Chainweb.Version as CW


-- | Check and assert that a chainweb 'CW.ChainId' is equal to
-- some pact 'P.ChainId'.
--
assertChainId :: CW.ChainId -> P.ChainId -> Bool
assertChainId cid0 cid1 = chainIdToText cid0 == P._chainId cid1
{-# INLINE assertChainId #-}

-- | Check and assert that 'GasPrice' is rounded to at most 12 decimal
-- places.
--
assertGasPrice :: GasPrice -> Bool
assertGasPrice (GasPrice (ParsedDecimal gp)) = decimalPlaces gp <= 12
{-# INLINE assertGasPrice #-}

-- | Check and assert that 'ChainwebVersion' is equal to some pact 'NetworkId'.
--
assertNetworkId :: ChainwebVersion -> P.NetworkId -> Bool
assertNetworkId v (P.NetworkId nid) = fromText @ChainwebVersion nid == Just v
{-# INLINE assertNetworkId #-}

-- | Check and assert that the number of signatures in a 'Command' is
-- at most 100.
--
assertSigSize :: [UserSig] -> Bool
assertSigSize sigs = length sigs <= 100
{-# INLINE assertSigSize #-}

-- | Check and assert that the initial 'Gas' cost of a transaction
-- is less than the specified 'GasLimit'.
--
assertTxSize :: Gas -> GasLimit -> Bool
assertTxSize initialGas gasLimit = initialGas < (fromIntegral gasLimit)
{-# INLINE assertTxSize #-}

-- | Check and assert a given 'BlockCreationTime' conforms to the time bounds
-- set in a given 'Commnad'.
--
-- Note: @prop_tx_ttl_newBlock/assertBlock@
--
assertBlockTime :: BlockCreationTime -> Command (Payload PublicMeta ParsedCode) -> Bool
assertBlockTime (BlockCreationTime blockOriginationTime) tx =
    ttl > 0
    && blockOriginationTime >= (toMicrosFromSeconds 0)
    && txOriginationTime >= 0
    && toMicrosFromSeconds txOriginationTime < blockOriginationTime
    && toMicrosFromSeconds (txOriginationTime + ttl) >= blockOriginationTime
    && ttl <= maxTTL
  where
    (TTLSeconds ttl) = timeToLiveOf tx
    toMicrosFromSeconds = Time . TimeSpan . Micros . fromIntegral . (1000000 *)
    (TxCreationTime txOriginationTime) = creationTimeOf tx
{-# INLINE assertBlockTime #-}

-- | Check and assert whether compilation succeeds for a given transaction
--
assertCompile :: Bool -> ChainwebTransaction -> Either InsertError ChainwebTransaction
assertCompile allowModuleInstall tx = case payload of
    Exec (ExecMsg parsedCode _) ->
      case compileCode parsedCode of
        Left perr -> Left $ InsertErrorCompilationFailed (sshow perr)
        Right terms | allowModuleInstall -> Right tx
                    | otherwise -> foldr bailOnModule (Right tx) terms
    _ -> Right tx
  where
    payload = _pPayload $ payloadObj $ _cmdPayload tx
    compileCode p = compileExps (mkTextInfo (_pcCode p)) (_pcExps p)
    bailOnModule (TModule {}) _ = Left $ InsertErrorCompilationFailed "Module/interface install not supported"
    bailOnModule _ b =  b
