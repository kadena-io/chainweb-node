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
-- Pact Service SPV support
--
module Chainweb.Pact.SPV
( -- * SPV Support
  noSPV
, pactSPV
  -- * SPV Api Utilities
, getTxIdx
) where


import GHC.Stack

import Control.Concurrent.MVar
import Control.Lens hiding (index)
import Control.Monad.Catch

import Data.Aeson hiding (Object, (.=))
import Data.Default (def)
import Data.Text (Text, pack)

import Numeric.Natural

import qualified Streaming.Prelude as S

-- internal chainweb modules

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.CutDB (CutDb)
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.SPV.VerifyProof
import Chainweb.TreeDB
import Chainweb.Utils

import Data.CAS

-- internal pact modules

import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.PactValue
import Pact.Types.Runtime hiding (ChainId)


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
    => MVar (CutDb cas)
    -> SPVSupport
pactSPV cdbv = SPVSupport $ \s o -> readMVar cdbv >>= spv s o
  where
    -- extract spv resources from pact object
    spv s o cdb = case s of
      "TXOUT" -> do
        t <- mkProof o
        case t of
          Left e -> spvMessage e
          Right u -> verifyTransactionOutputProof cdb u >>= txToJson
      "TXIN" -> spvMessage "TXIN is currently unsupported"
      x -> spvMessage
        $ "TXIN/TXOUT must be specified to generate valid spv proofs: "
        <> x

    mkProof o =
      let
        k a = case fromJSON . toJSON $ a of
          Error e -> spvMessage' e
          Success u -> pure $ Right u
      in either spvMessage k . toPactValue $ TObject o def

    txToJson (TransactionOutput t) =
      case decodeStrict t of
        Nothing ->
          -- this should be considered an internal error
          internalError "Unable to decode spv transaction output"
        Just (HashedLogTxOutput u _) ->
          case fromJSON u of
            Error e -> spvMessage' e
            Success (CommandSuccess (TObject o _)) -> pure $ Right o
            Success _ -> internalError $
              -- if we get here, then the proof is corrupted
              "Associated pact transaction has wrong format"

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
        $ entries bdb Nothing (Just 1) (Just $ int bh) Nothing S.head_ >>= \case
            Nothing -> spvMessage "unable to find payload associated with transaction hash"
            Just x -> pure . Right $ x

    case ph of
      Left s -> pure . Left $ s
      Right a -> do
        -- get payload
        payload <- _payloadWithOutputsTransactions <$> casLookupM pdb a

        -- Find transaction index
        r <- S.each payload
          & S.map fst
          & S.mapM toTxHash
          & sindex (== th)

        case r of
          Nothing -> spvMessage "unable to find transaction at the given block height"
          Just x -> return . Right $ int x
  where
    toPactTx :: MonadThrow m => Transaction -> m (Command Text)
    toPactTx (Transaction b) = decodeStrictOrThrow b

    toTxHash :: MonadThrow m => Transaction -> m PactHash
    toTxHash = fmap _cmdHash . toPactTx

    sfind :: Monad m => (a -> Bool) -> S.Stream (S.Of a) m () -> m (Maybe a)
    sfind p = S.head_ . S.dropWhile (not . p)

    sindex :: Monad m => (a -> Bool) -> S.Stream (S.Of a) m () -> m (Maybe Natural)
    sindex p s = S.zip (S.each [0..]) s & sfind (p . snd) & fmap (fmap fst)

-- -------------------------------------------------------------------------- --
-- utilities

-- | Prepend "spvSupport" to any errors so we can differentiate messages
--
spvMessage :: Text -> IO (Either Text a)
spvMessage = pure . Left . (<>) "spvSupport: "

spvMessage' :: String -> IO (Either Text a)
spvMessage' = spvMessage . pack
