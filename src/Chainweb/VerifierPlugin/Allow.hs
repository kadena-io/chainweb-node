{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
module Chainweb.VerifierPlugin.Allow(plugin) where

import Control.Monad
import Control.Monad.Except
import Data.Aeson
import qualified Data.Set as Set
import qualified Data.Text.Encoding as Text

import Pact.Core.PactValue
import Pact.Core.Errors (VerifierError(..))

import Chainweb.VerifierPlugin
import Pact.Core.Signer (SigCapability)
import Pact.Core.Gas

-- This trivial verifier plugin takes as its "proof" a JSON-encoded capability,
-- and grants only that capability.
plugin :: VerifierPlugin
plugin = VerifierPlugin $ \_ proof caps gasRef -> do
    chargeGas gasRef (Gas 100)
    decodedCap <- decodeArgToCap proof
    unless (caps == Set.singleton decodedCap) $
        throwError $ VerifierError "granted capability is not the one in the proof"
    where
    decodeArgToCap :: MonadError VerifierError m => PactValue -> m SigCapability
    decodeArgToCap (PString arg) =
        case eitherDecodeStrict' (Text.encodeUtf8 arg) of
            Left _err -> throwError $ VerifierError $ "argument was not a JSON-encoded capability: " <> arg
            Right cap -> return cap
    decodeArgToCap _ =
        throwError $ VerifierError "expected string literal in verifier arguments"
