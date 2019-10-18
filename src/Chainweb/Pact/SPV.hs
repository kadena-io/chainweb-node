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

import Control.Concurrent.MVar
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

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.CutDB (CutDb)
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types
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
    :: forall cas
    . CW.ChainId
      -- ^ chain id of the running pact service
    -> MVar (CutDb cas)
      -- ^ handle into the cutdb
    -> SPVSupport
pactSPV cid cdbv = SPVSupport (verifySPV cid cdbv) (verifyCont cid cdbv)

-- | SPV transaction verification support. Calls to 'verify-spv' in Pact
-- will thread through this function and verify an SPV receipt, making the
-- requisite calls to the SPV api and verifying the output proof.
--
verifySPV
    :: forall cas
    . CW.ChainId
    -> MVar (CutDb cas)
      -- ^ handle into the cut db
    -> Text
      -- ^ TXOUT or TXIN - defines the type of proof
      -- used in validation
    -> Object Name
      -- ^ the 'TransactionOutputProof' object to validate
    -> IO (Either Text (Object Name))
verifySPV cid cdbv typ proof = readMVar cdbv >>= go typ proof
  where
    go s o cdb = case s of
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

            TransactionOutput p <- verifyTransactionOutputProof cdb u

            q <- case decodeStrict' p :: Maybe HashCommandResult of
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
    :: forall cas
    . CW.ChainId
    -> MVar (CutDb cas)
      -- ^ handle into the cut db
    -> ContProof
      -- ^ bytestring of 'TransactionOutputP roof' object to validate
    -> IO (Either Text PactExec)
verifyCont cid cdbv (ContProof cp) = do
    cdb <- readMVar cdbv
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

          TransactionOutput p <- verifyTransactionOutputProof cdb u

          q <- case decodeStrict' p :: Maybe HashCommandResult of
            Nothing -> internalError "unable to decode spv transaction output"
            Just cr -> return cr

          r <- case _crContinuation q of
            Nothing -> return $! Left "no pact exec found in command result"
            Just pe -> return $! Right pe

          return r

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
