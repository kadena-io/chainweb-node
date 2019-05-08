{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module: Chainweb.Pact.PactService
-- Copyright: Copyright © 2018 Kadena LLC.
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
import Control.Lens
import Control.Monad.Catch

import Data.Aeson hiding (Object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Default (def)

import Crypto.Hash.Algorithms

-- internal chainweb modules

import Chainweb.CutDB (CutDb)
import Chainweb.Pact.Service.Types
import Chainweb.Payload
import Chainweb.SPV
import Chainweb.SPV.VerifyProof

-- internal pact modules

import Pact.Types.Command
import Pact.Types.Runtime


-- -------------------------------------------------------------------------- --
-- Convenient types

type OutputProof = TransactionOutputProof SHA512t_256
type InputProof = TransactionProof SHA512t_256

-- -------------------------------------------------------------------------- --
-- Pact SPV functionality

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
      --
      -- 1. Create JSON-formatted input or output proof from
      -- pact object (KV-pairs of keys and pact types as avlues)
      --
      -- 2. Verify the output/input via the SPV api. This will
      -- produce proof that a transaction input/output occurred.
      -- If this fails, it will be returned as a failed command
      -- along with the relevant error message.
      --
      -- 3. Create a Pact 'CommandSuccess' object and return
      -- the successfully verified proof object.
      t <- proofOf @OutputProof o
      (TransactionOutput u) <- verifyTransactionOutputProof cdb t
      supportOf u

    txin cdb o = do
      t <- proofOf @InputProof o
      (Transaction u) <- verifyTransactionProof cdb t
      supportOf u

    -- retrieve the spv object from 'CommandSuccess'
    supportOf v = Right . _tObject . _csData <$> mkSuccess v

-- | Obtain an SPV proof given a Pact object
--
-- Usage is best with -XTypeApplications, like so:
--
-- 'proofOf' '@(Transaction SHA512t_256)'
--
proofOf
    :: (FromJSON a, MonadThrow m)
    => Object Name -> m a
proofOf = spvDecode . toJSON . (flip TObject) def

-- | Internal method: decode some JSON value as 'FromJSON a => a'.
--
-- Usage is best with type applications as in 'proofOf':
-- the applied type propagates to this function.
--
-- 'spvDecode' '@OutputProof'
--
spvDecode
    :: (FromJSON a, MonadThrow m)
    => Value -> m a
spvDecode a = case fromJSON a of
    Error s -> spvError $ "Unable to decode tx: " <> s
    Success x -> pure x

-- | Produce a Pact 'CommandSuccess' of a transaction proof bytestring.
--
mkSuccess
    :: MonadThrow m
    => ByteString
    -- ^ Transaction verification proof output string
    -> m (CommandSuccess (Term Name))
mkSuccess
    = maybe (spvError err) pure
    . decode @(CommandSuccess (Term Name))
    . fromStrict
  where
    err = "Unable to decode bytestring as CommandSuccess"

-- -------------------------------------------------------------------------- --
-- Nicely formatted errors

-- | Prepend "spvSupport" to any errors so we can differentiate
--
spvError :: MonadThrow m => String -> m a
spvError = internalError' . (<>) "spvSupport: "
