{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

-- |
-- Module: Chainweb.Pact.PactService
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: See LICENSE file
-- Maintainers: Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- Pact Service SPV support
--
module Chainweb.Pact.SPV
( -- * spv support
  pactSPV
, verifySPV
, verifyCont
  -- * spv api utilities
, getTxIdx
) where


import GHC.Stack

import Control.Error
import Control.Lens hiding (index)
import Control.Monad.Catch

import Data.Aeson hiding (Object, (.=))
import Data.Default (def)
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as T

import Crypto.Hash.Algorithms

import Numeric.Natural

import qualified Streaming.Prelude as S

-- internal chainweb modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeight
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Utils (aeson)
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.SPV
import Chainweb.SPV.VerifyProof
import Chainweb.TreeDB
import Chainweb.Utils
import qualified Chainweb.Version as CW

import Data.CAS

-- internal pact modules

import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.PactValue
import Pact.Types.Runtime
import Pact.Types.SPV


-- | Spv support for pact
--
pactSPV
    :: BlockHeaderDb
      -- ^ handle into the cutdb
    -> BlockHash
      -- ^ the context for verifying the proof
    -> SPVSupport
pactSPV bdb bh = SPVSupport (verifySPV bdb bh) (verifyCont bdb bh)

-- | SPV transaction verification support. Calls to 'verify-spv' in Pact
-- will thread through this function and verify an SPV receipt, making the
-- requisite calls to the SPV api and verifying the output proof.
--
verifySPV
    :: BlockHeaderDb
      -- ^ handle into the cut db
    -> BlockHash
        -- ^ the context for verifying the proof
    -> Text
      -- ^ TXOUT or TXIN - defines the type of proof
      -- used in validation
    -> Object Name
      -- ^ the 'TransactionOutputProof' object to validate
    -> IO (Either Text (Object Name))
verifySPV bdb bh typ proof = go typ proof
  where
    cid = CW._chainId bdb
    go s o = case s of
      "TXOUT" -> case extractProof o of
        Left t -> return (Left t)
        Right u
          | (view outputProofChainId u) /= cid ->
            internalError "cannot redeem spv proof on wrong target chain"
          | otherwise -> do

            -- SPV proof verification is a 3 step process:
            --
            --  1. verify spv tx output proof via chainweb spv api
            --
            --  2. Decode tx outputs to 'HashCommandResult'
            --
            --  3. Extract tx outputs as a pact object and return the
            --  object.

            TransactionOutput p <- verifyTransactionOutputProofAt_ bdb u bh

            q <- case decodeStrict' p :: Maybe (CommandResult Hash) of
              Nothing -> internalError "unable to decode spv transaction output"
              Just cr -> return cr

            r <- case _crResult q of
              PactResult Left{} ->
                return $! Left "invalid command result in tx output proof"
              PactResult (Right v) -> case fromPactValue v of
                TObject !j _ -> return $! Right j
                _ -> return $ Left "spv-verified tx output has invalid type"

            return r

      t -> return . Left $! "unsupported SPV types: " <> t

-- | SPV defpact transaction verification support. This call validates a pact 'endorsement'
-- in Pact, providing a validation that the yield data of a cross-chain pact is valid.
--
verifyCont
    :: BlockHeaderDb
      -- ^ handle into the cut db
    -> BlockHash
        -- ^ the context for verifying the proof
    -> ContProof
      -- ^ bytestring of 'TransactionOutputP roof' object to validate
    -> IO (Either Text PactExec)
verifyCont bdb bh (ContProof cp) = do
    t <- decodeB64UrlNoPaddingText $ T.decodeUtf8 cp
    case decodeStrict' t of
      Nothing -> internalError "unable to decode continuation proof"
      Just u
        | (view outputProofChainId u) /= cid ->
          internalError "cannot redeem continuation proof on wrong target chain"
        | otherwise -> do

          -- Cont proof verification is a 3 step process:
          --
          --  1. verify spv tx output proof via chainweb spv api
          --
          --  2. Decode tx outputs to 'HashCommandResult'
          --
          --  3. Extract continuation 'PactExec' from decoded result
          --  and return the cont exec object

          TransactionOutput p <- verifyTransactionOutputProofAt_ bdb u bh

          q <- case decodeStrict' p :: Maybe (CommandResult Hash) of
            Nothing -> internalError "unable to decode spv transaction output"
            Just cr -> return cr

          r <- case _crContinuation q of
            Nothing -> return $! Left "no pact exec found in command result"
            Just pe -> return $! Right pe

          return r
  where
    cid = CW._chainId bdb

-- | Extract a 'TransactionOutputProof' from a generic pact object
--
extractProof :: Object Name -> Either Text (TransactionOutputProof SHA512t_256)
extractProof o = toPactValue (TObject o def) >>= k
  where
    k = aeson (Left . pack) Right
      . fromJSON
      . toJSON

-- | Look up pact tx hash at some block height in the
-- payload db, and return the tx index for proof creation.
--
-- Note: runs in O(n) - this should be revisited if possible
--
getTxIdx
    :: HasCallStack
    => PayloadCas cas
    => BlockHeaderDb
    -> PayloadDb cas
    -> BlockHeight
    -> PactHash
    -> IO (Either Text Int)
getTxIdx bdb pdb bh th = do
    -- get BlockPayloadHash
    ph <- fmap (fmap _blockPayloadHash)
        $ entries bdb Nothing (Just 1) (Just $ int bh) Nothing S.head_
        >>= pure . note "unable to find payload associated with transaction hash"

    case ph of
      (Left !s) -> return $! Left s
      (Right !a) -> do
        -- get payload
        payload <- _payloadWithOutputsTransactions <$> casLookupM pdb a

        -- Find transaction index
        r <- S.each payload
          & S.map fst
          & S.mapM toTxHash
          & sindex (== th)

        r & note "unable to find transaction at the given block height"
          & fmap int
          & return
  where
    toPactTx :: MonadThrow m => Transaction -> m (Command Text)
    toPactTx (Transaction b) = decodeStrictOrThrow' b

    toTxHash :: MonadThrow m => Transaction -> m PactHash
    toTxHash = fmap _cmdHash . toPactTx

    sfind :: Monad m => (a -> Bool) -> S.Stream (S.Of a) m () -> m (Maybe a)
    sfind p = S.head_ . S.dropWhile (not . p)

    sindex :: Monad m => (a -> Bool) -> S.Stream (S.Of a) m () -> m (Maybe Natural)
    sindex p s = S.zip (S.each [0..]) s & sfind (p . snd) & fmap (fmap fst)
