{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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

import Data.CAS

-- internal pact modules

import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.Logger
import Pact.Types.PactValue
import Pact.Types.Runtime
import Pact.Types.SPV


-- -------------------------------------------------------------------------- --
-- Noop and std pact spv support

-- | Spv support for pact
--
pactSPV :: forall cas. MVar (CutDb cas) -> Logger -> SPVSupport
pactSPV cdbv l = SPVSupport (verifySPV cdbv l) (verifyCont cdbv l)

-- | SPV transaction verification support. Calls to 'verify-spv' in Pact
-- will thread through this function and verify an SPV receipt, making the
-- requisite calls to the SPV api and verifying the output proof.
--
verifySPV
    :: forall cas
    . MVar (CutDb cas)
    -> Logger
    -> Text
    -> Object Name
    -> IO (Either Text (Object Name))
verifySPV cdbv l typ proof = readMVar cdbv >>= go typ proof
  where
    -- extract spv resources from pact object
    go s o cdb = case s of
      "TXOUT" -> case txOutputProofOf o of
        (Left !u) -> return $! Left u
        Right t -> extractOutputs =<< verifyTransactionOutputProof cdb t
      "TXIN" -> return $ Left "TXIN is currently unsupported"
      x -> return . Left
        $ "TXIN or TXOUT must be specified to generate valid spv proofs: "
        <> x

    txOutputProofOf
        :: Object Name
        -> Either Text (TransactionOutputProof SHA512t_256)
    txOutputProofOf o = k =<< toPactValue (TObject o def)
      where
        k = aeson (Left . pack) Right . fromJSON . toJSON

    extractOutputs :: TransactionOutput -> IO (Either Text (Object Name))
    extractOutputs (TransactionOutput t) =
      case decodeStrict' t :: Maybe HashCommandResult of
        Nothing -> internalError $
          "unable to decode spv transaction output"
        Just (CommandResult _ _ (PactResult (Right pv)) _ _ _ _) -> case fromPactValue pv of
          (TObject !o _) -> return $! Right o
          !o -> do
            logLog l "ERROR" $ show o
            return . Left $ pack "type error in associated pact transaction, should be object"
        Just o -> do
          logLog l "ERROR" $ show o
          return . Left $ pack "Invalid command result in associated pact output"

-- | SPV defpact transaction verification support. This call validates a pact 'endorsement'
-- in Pact, providing a validation that the yield data of a cross-chain pact is valid.
--
verifyCont
    :: forall cas
    . MVar (CutDb cas)
    -> Logger
    -> ContProof
    -> IO (Either Text PactExec)
verifyCont _cdbv _l = _spvVerifyContinuation noSPVSupport

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
