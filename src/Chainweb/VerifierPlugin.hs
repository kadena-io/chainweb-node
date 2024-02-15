{-# language BangPatterns #-}
{-# language DerivingStrategies #-}
{-# language EmptyCase #-}
{-# language FlexibleContexts #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language TypeApplications #-}
{-# language RankNTypes #-}

module Chainweb.VerifierPlugin
    ( VerifierPlugin(..)
    , VerifierError(..)
    , runVerifierPlugins
    , chargeGas
    ) where

import Control.DeepSeq
import Control.Exception.Safe(Exception, throw, try, tryAny)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Morph (hoist)
import Control.Monad.ST
import Control.Monad.Trans.Class
import System.LogLevel

import Data.Foldable
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Merge
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as Set
import Data.STRef
import Data.Text(Text)

import Pact.Types.Capability
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Gas
import Pact.Types.PactValue
import Pact.Types.Verifier

import Chainweb.Logger
import Chainweb.Utils

newtype VerifierError = VerifierError
    { getVerifierError :: Text }
    deriving stock Show
instance Exception VerifierError

newtype VerifierPlugin
    = VerifierPlugin
    { runVerifierPlugin
        :: forall s
        . PactValue
        -> Set SigCapability
        -> STRef s Gas
        -> ExceptT VerifierError (ST s) ()
    }
instance NFData VerifierPlugin where
    rnf !_ = ()

chargeGas :: STRef s Gas -> Gas -> ExceptT VerifierError (ST s) ()
chargeGas r g = do
    gasRemaining <- lift $ readSTRef r
    when (g < 0) $ throwError $ VerifierError $
        "verifier attempted to charge negative gas amount: " <> sshow g
    when (g > gasRemaining) $ throwError $ VerifierError $
        "gas exhausted in verifier. attempted to charge " <> sshow (case g of Gas g' -> g') <>
        " with only " <> sshow (case gasRemaining of Gas gasRemaining' -> gasRemaining') <> " remaining."
    lift $ writeSTRef r (gasRemaining - g)

runVerifierPlugins :: Logger logger => logger -> Map VerifierName VerifierPlugin -> Gas -> Command (Payload PublicMeta ParsedCode) -> IO (Either VerifierError Gas)
runVerifierPlugins logger allVerifiers gasRemaining tx = try $ do
    gasRef <- stToIO $ newSTRef gasRemaining
    either throw (\_ -> stToIO $ readSTRef gasRef) <=< runExceptT $ Merge.mergeA
        -- verifier in command does not exist in list of all valid verifiers
        (Merge.traverseMissing $ \(VerifierName vn) _ ->
            throwError $ VerifierError ("verifier does not exist: " <> vn)
        )
        -- valid verifier not used in command
        Merge.dropMissing
        (Merge.zipWithAMatched $ \(VerifierName vn) proofsAndCaps verifierPlugin ->
            for_ proofsAndCaps $ \(ParsedVerifierProof proof, caps) -> do
                verifierGasRemaining <- lift $ stToIO $ readSTRef gasRef
                tryAny (hoist stToIO (runVerifierPlugin verifierPlugin proof caps gasRef)) >>= \case
                    Left ex -> do
                        liftIO $ logFunctionText logger Warn ("Uncaught exception in verifier: " <> sshow ex)
                        throwError $ VerifierError "Uncaught exception in verifier"
                    Right () -> return ()
                verifierDoneGasRemaining <- lift $ stToIO $ readSTRef gasRef
                if verifierDoneGasRemaining > verifierGasRemaining
                then throwError $ VerifierError ("Verifier attempted to charge negative gas: " <> vn)
                else return ()
            )
        verifiersInCommand
        allVerifiers
    where
    verifiersInCommand :: Map VerifierName [(ParsedVerifierProof, Set SigCapability)]
    verifiersInCommand =
        Map.fromListWith (++) $
        fmap
            (\Verifier {_verifierName = name, _verifierProof = proof, _verifierCaps = caps} ->
                (name, [(proof, Set.fromList caps)])) $
        fromMaybe [] $ _pVerifiers (_cmdPayload tx)
