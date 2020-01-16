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
( validateChainId
, validateGasPrice
, validateNetworkId
, validateBlockTime
, validateSigSize
, validateTxSize
, validateCompile
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


-- | Check and validate that a chainweb 'CW.ChainId' is equal to
-- some pact 'P.ChainId'.
--
validateChainId :: CW.ChainId -> P.ChainId -> Bool
validateChainId cid0 cid1 = chainIdToText cid0 == P._chainId cid1
{-# INLINE validateChainId #-}

-- | Check and validate that 'GasPrice' is rounded to at most 12 decimal
-- places.
--
validateGasPrice :: GasPrice -> Bool
validateGasPrice (GasPrice (ParsedDecimal gp)) = decimalPlaces gp <= 12
{-# INLINE validateGasPrice #-}

-- | Check and validate that 'ChainwebVersion' is equal to some pact 'NetworkId'.
--
validateNetworkId :: ChainwebVersion -> P.NetworkId -> Bool
validateNetworkId v (P.NetworkId nid) = fromText @ChainwebVersion nid == Just v
{-# INLINE validateNetworkId #-}

-- | Check and validate that the number of signatures in a 'Command' is
-- at most 100.
--
validateSigSize :: [UserSig] -> Bool
validateSigSize sigs = length sigs <= 100
{-# INLINE validateSigSize #-}

-- | Check and validate that the initial 'Gas' cost of a transaction
-- is less than the specified 'GasLimit'.
--
validateTxSize :: Gas -> GasLimit -> Bool
validateTxSize initialGas gasLimit = initialGas < (fromIntegral gasLimit)
{-# INLINE validateTxSize #-}

-- | Check and validate a given 'BlockCreationTime' conforms to the time bounds
-- set in a given 'Commnad'.
--
-- Note: @prop_tx_ttl_newBlock/validateBlock@
--
validateBlockTime :: BlockCreationTime -> Command (Payload PublicMeta ParsedCode) -> Bool
validateBlockTime (BlockCreationTime blockOriginationTime) tx =
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
{-# INLINE validateBlockTime #-}

-- | Check and validate whether compilation succeeds for a given transaction
--
validateCompile :: Bool -> ChainwebTransaction -> Either InsertError ChainwebTransaction
validateCompile allowModuleInstall tx = case payload of
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
{-# INLINE validateCompile #-}
