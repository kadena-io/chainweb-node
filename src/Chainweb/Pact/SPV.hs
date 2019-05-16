{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Pact.PactService
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: See LICENSE file
-- Maintainers: Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- Pact Service SPV support and internal resources
--
module Chainweb.Pact.SPV
( noSPV
, pactSPV
) where


import GHC.Stack

import Control.Concurrent.MVar
import Control.Lens hiding (index)
import Control.Monad.Catch

import Data.Aeson hiding (Object, (.=))
import Data.Default (def)

import Crypto.Hash.Algorithms

import Numeric.Natural

import qualified Streaming.Prelude as S

-- internal chainweb modules

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.CutDB (CutDb)
import Chainweb.Pact.Service.Types
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
import Pact.Types.PactValue
import Pact.Types.Runtime hiding (ChainId)
import Pact.Types.Term


-- -------------------------------------------------------------------------- --
-- Noop and std pact spv support

-- | No-op SPV support (used for testing and stubs)
--
noSPV :: SPVSupport
noSPV = noSPVSupport

fromRight :: Either a b -> IO b
fromRight (Right b) = pure b
fromRight (Left a) = spvError "REEEEEE"

-- | Spv support for pact
--
pactSPV
    :: HasCallStack
    => MVar (CutDb cas)
      -- ^ a handle to the the cut db to look up tx proofs
    -> SPVSupport
pactSPV cdbv =
    -- SPVSupport :: Text -> Object Name -> IO (Either Text (Object Name))
    SPVSupport $ \s o -> readMVar cdbv >>= spv s o
  where
    -- extract spv resources from pact object
    spv s (Object (ObjectMap om) _ _ _) cdb = case s of
      "TXOUT" -> do

        o <- case om ^. at (FieldKey "spv-proof") of
          Nothing -> spvError "no proof object found"
          Just (TObject a _) -> pure a
          Just t -> spvError
            $ "proof object is wrong format: "
            <> show t

        t <- mkProof o
        u <- verifyTransactionProof cdb t
        v <- txToJSON u
        pure $! Right v
      "TXIN" -> spvError "TXIN is currently unsupported"

      x -> pure . Left
        $ "TXIN/TXOUT must be specified to generate a valid spv: "
        <> x

    mkProof :: Object Name -> IO (TransactionProof SHA512t_256)
    mkProof o = do
      t <- fromRight . toPactValue $ TObject o def
      let r = fromJSON . toJSON $ t
      case r of
        Error e -> spvError e
        Success u -> pure u

    txToJSON :: Transaction -> IO (Object Name)
    txToJSON tx =
      let
        r = fromJSON . toJSON $ tx
      in case r of
        Error e -> spvError e
        Success t -> pure t


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
    -> IO Int
getTxIdx bdb pdb bh th = do
    -- get BlockPayloadHash
    ph <- fmap _blockPayloadHash
        $ entries bdb Nothing (Just 1) (Just $ int bh) Nothing S.head_ >>= \case
            Nothing -> spvError "unable to find payload associated with transaction hash"
            Just x -> return x

    -- Get payload
    payload <- _payloadWithOutputsTransactions <$> casLookupM pdb ph

    -- Find transaction index
    r <- S.each payload
        & S.map fst
        & S.mapM toTxHash
        & index (== th)

    case r of
        Nothing -> spvError "unable to find transaction at the given block height"
        Just x -> return (int x)
  where
    toPactTx :: MonadThrow m => Transaction -> m (Command Text)
    toPactTx (Transaction b) = decodeStrictOrThrow b

    toTxHash :: MonadThrow m => Transaction -> m PactHash
    toTxHash = fmap _cmdHash . toPactTx

    find :: Monad m => (a -> Bool) -> S.Stream (S.Of a) m () -> m (Maybe a)
    find p = S.head_ . S.dropWhile (not . p)

    index :: Monad m => (a -> Bool) -> S.Stream (S.Of a) m () -> m (Maybe Natural)
    index p s = S.zip (S.each [0..]) s & find (p . snd) & fmap (fmap fst)

-- | Prepend "spvSupport" to any errors so we can differentiate
--
spvError :: forall a. String -> IO a
spvError = internalError' . (<>) "spvSupport: "
