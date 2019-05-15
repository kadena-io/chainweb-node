{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module: Chainweb.Pact.PactService
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: See LICENSE file
-- Maintainers: Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- Pact Service SPV support and utils
module Chainweb.Pact.SPV
( noSPV
, pactSPV
) where


import GHC.Generics hiding (to)
import GHC.Stack

import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Lens hiding (index)
import Control.Monad (unless)
import Control.Monad.Catch

import Data.Aeson hiding (Object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Default (def)
import Data.Maybe (isJust)
import Data.Text (unpack)

import Crypto.Hash.Algorithms

import Numeric.Natural

import qualified Streaming.Prelude as S

-- internal chainweb modules

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.CutDB (CutDb, cutDbBlockHeaderDb)
import Chainweb.Pact.Service.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.SPV
import Chainweb.SPV.CreateProof
import Chainweb.SPV.VerifyProof
import Chainweb.TreeDB
import Chainweb.Utils

import Data.CAS

-- internal pact modules

import Pact.Types.Hash
import Pact.Types.Command
import Pact.Types.Runtime hiding (ChainId)


-- -------------------------------------------------------------------------- --
-- spv data (internal use only)

newtype DeleteChainId = DeleteChainId { _deleteChainId :: ChainId }
  deriving (Eq, Show, Generic)
  deriving newtype NFData

deleteChainId :: Getter DeleteChainId ChainId
deleteChainId = to _deleteChainId

newtype TargetChainId = TargetChainId { _targetChainId :: ChainId }
  deriving (Eq, Show, Generic)
  deriving newtype NFData

targetChainId :: Getter TargetChainId ChainId
targetChainId = to _targetChainId

data SpvResources = SpvResources
  { _spvDeleteChainId :: DeleteChainId
  , _spvTargetChainId :: TargetChainId
  , _spvBlockHeight :: BlockHeight
  , _spvPactHash :: PactHash
  }

spvDeleteChainId :: Getter SpvResources DeleteChainId
spvDeleteChainId = to _spvDeleteChainId

spvTargetChainId :: Getter SpvResources TargetChainId
spvTargetChainId = to _spvTargetChainId

spvBlockHeight :: Getter SpvResources BlockHeight
spvBlockHeight = to _spvBlockHeight

spvPactHash :: Getter SpvResources PactHash
spvPactHash = to _spvPactHash

fromTransactionProof :: Iso' (Object Name) (TransactionProof SHA512t_256)
fromTransactionProof = undefined

fromTransactionOutputProof :: Iso' (Object Name) (TransactionOutputProof SHA512t_256)
fromTransactionOutputProof = undefined

-- -------------------------------------------------------------------------- --
-- Noop and std pact spv support

-- | No-op SPV support (used for testing and stubs)
--
noSPV :: SPVSupport
noSPV = noSPVSupport

-- | Spv support for pact
--
pactSPV
    :: HasCallStack
    => PayloadCas cas
    => MVar (CutDb cas)
      -- ^ a handle to the the cut db to look up tx proofs
    -> PayloadDb cas
      -- ^ a handle to the payload db to look up tx ixes
    -> SPVSupport
pactSPV cdbv pdb =
    -- SPVSupport :: Text -> Object Name -> IO (Either Text (Object Name))
    SPVSupport $ \s o -> readMVar cdbv >>= spv s o
  where
    -- extract spv resources from pact object
    spv s o cdb =
      withSpvResources cdb pdb o extract $ go s cdb

    -- create and verify proofs via chainweb api
    go s cdb (SpvResources cid tid bh _) tix =
      case s of
        "TXIN"  ->
          error "TODO: create TransactionProof pact :: IO (Either Text (Object Name))"
        "TXOUT" -> do
          error "TODO: create TransactionOutputProof pact object :: IO (Either Text (Object Name))"
        t -> pure . Left
          $ "TXIN/TXOUT must be specified to generate a valid spv: "
          <> t

    extract = error "TODO: extract data from pact object"

-- | Handle that allows us to work with the cutdb
-- and the payload db for a given proof subject
--
withSpvResources
    :: PayloadCas cas
    => CutDb cas
      -- ^ cutdb can retrieve blockheader db and generate spv
      -- transaction proofs
    -> PayloadDb cas
      -- ^ used to look up tx index along with block header db
    -> Object Name
      -- ^ the SPV object to verify
    -> (Object Name -> IO SpvResources)
      -- ^ the action that extracts relevant data from an object
    -> (SpvResources -> Int -> IO a)
      -- ^ the action that produces the final spv result
    -> IO a
withSpvResources cdb pdb o extract act = do
  t <- extract o

  let cid = t ^. spvDeleteChainId . deleteChainId
      bh  = t ^. spvBlockHeight
      ph  = t ^. spvPactHash

  bdb <- case cdb ^? cutDbBlockHeaderDb cid of
    Nothing -> spvError
      $ "no blockheader db found for chain id: "
      <> show cid
    Just a -> pure a

  tix <- getTxIdx bdb pdb bh ph

  act t tix

-- | Look up pact tx hash at some block height in the
-- payload db, and return the tx index for proof creation.
--
-- Note: runs in O(n) - this should be revisited if possible
--
getTxIdx
    :: PayloadCas cas
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
spvError :: String -> IO a
spvError = internalError' . (<>) "spvSupport: "
