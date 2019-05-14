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
-- Pact Service SPV support and utils
module Chainweb.Pact.SPV
( noSPV
, pactSPV
) where


import Control.Concurrent.MVar
import Control.Monad.Catch

import Data.Aeson hiding (Object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Default (def)
import qualified Data.Sequence as Seq

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

import Pact.Types.Hash
import Pact.Types.Command
import Pact.Types.Runtime


-- the type of transaction output proofs i.e. TXOUT
type OutputProof = TransactionOutputProof SHA512t_256

-- the type of transaction input proofs i.e. TXIN
type InputProof = TransactionProof SHA512t_256

-- -------------------------------------------------------------------------- --
-- Noop and std pact spv support

noSPV :: SPVSupport
noSPV = noSPVSupport

pactSPV :: MVar (CutDb cas) -> SPVSupport
pactSPV dbVar = SPVSupport $ \s o -> do
    cutDb <- readMVar dbVar
    -- apply the functions 'txin' or 'txout' in the case
    -- where an input or output are requested
    case s of
      "TXOUT" -> txout cutDb o
      "TXIN" -> txin cutDb o
      _ -> pure $ Left "spvSupport: Unsupported SPV mode"
  where
    txout cdb o = do
      -- The strategy is this:

      -- 1. Create JSON-formatted input or output proof from
      -- pact object (KV-pairs of keys and pact types as values)
      t <- mkProof @OutputProof o

      -- 2. Verify the output/input via the SPV api. This will
      -- produce proof that a transaction input/output occurred.
      -- If this fails, it will be returned as a failed command
      -- along with the relevant error message.
      (TransactionOutput u) <- verifyTransactionOutputProof cdb t

      -- 3. Create a Pact 'CommandSuccess'f object and return
      -- the successfully verified proof object.
      supportOf u

    txin cdb o = do
      t <- mkProof @InputProof o
      (Transaction u) <- verifyTransactionProof cdb t
      supportOf u

    -- retrieve the spv object from 'CommandSuccess'
    supportOf v = Right . _tObject . _csData <$> mkSuccess v

-- -------------------------------------------------------------------------- --
-- utilities

-- | Obtain an SPV proof given a Pact object
--
-- Usage is best with -XTypeApplications, like so:
--
-- 'mkProof' '@(Transaction SHA512t_256)'
--
mkProof :: FromJSON a => Object Name -> IO a
mkProof o = spvDecode . toJSON $ TObject o def
  where
    spvDecode a = case fromJSON a of
      Error s -> spvError $ "unable to decode proof subject: " <> s
      Success x -> pure x

-- | Produce a Pact 'CommandSuccess' of a transaction proof bytestring.
--
mkSuccess :: ByteString -> IO (CommandSuccess (Term Name))
mkSuccess
    = maybe (spvError err) pure
    . decode @(CommandSuccess (Term Name))
    . fromStrict
  where
    err = "unable to decode proof object"

-- | Prepend "spvSupport" to any errors so we can differentiate
--
spvError :: String -> IO a
spvError = internalError' . (<>) "spvSupport: "

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
    -> IO Natural
getTxIdx bdb pdb bh th = do
    -- get BlockPayloadHash
    ph <- fmap _blockPayloadHash
        $ entries bdb Nothing (Just 1) (Just $ int bh) Nothing S.head_ >>= \case
            Nothing -> spvError "unable to find payload associated with transaction hash"
            Just x -> return x

    -- Get transactions
    txs <- fmap fst . _payloadWithOutputsTransactions <$> casLookupM pdb ph
    cmds <- traverse toTxHash txs

    -- find hash in txs
    case Seq.findIndexL (== th) cmds of
        Nothing -> spvError "unable to find transaction at the given block height"
        Just x -> return $ int x
  where
    toPactTx :: MonadThrow m => Transaction -> m (Command Text)
    toPactTx (Transaction b) = decodeStrictOrThrow b

    toTxHash :: MonadThrow m => Transaction -> m PactHash
    toTxHash = fmap _cmdHash . toPactTx
