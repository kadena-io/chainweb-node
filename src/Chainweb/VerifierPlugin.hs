{-# language BangPatterns #-}
{-# language DerivingStrategies #-}
{-# language EmptyCase #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language TypeApplications #-}
{-# language RankNTypes #-}

module Chainweb.VerifierPlugin
    ( VerifierPlugin(..)
    , VerifierError(..)
    , runVerifierPlugins
    , chargeGas
    , ShouldRunVerifierPlugins(..)
    ) where

import Control.DeepSeq
import Control.Exception.Safe(Exception, throw, try)
import Control.Monad
import Control.Monad.ST
import Control.Monad.Except
import Control.Monad.Trans.Class

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

import Chainweb.Utils

newtype VerifierError = VerifierError
    { getVerifierError :: Text }
    deriving stock Show
instance Exception VerifierError

data ShouldRunVerifierPlugins = RunVerifierPlugins | DoNotRunVerifierPlugins

newtype VerifierPlugin
    = VerifierPlugin
    { runVerifierPlugin
        :: forall s
        . PactValue
        -> Set SigCapability
        -> STRef s GasLimit
        -> ExceptT VerifierError (ST s) ()
    }
instance NFData VerifierPlugin where
    rnf !_ = ()

chargeGas :: STRef s GasLimit -> Gas -> ExceptT VerifierError (ST s) ()
chargeGas r g = do
    gl <- lift $ readSTRef r
    when (g < 0) $ throwError $ VerifierError $
        "verifier attempted to charge negative gas amount: " <> sshow g
    when (fromIntegral g > gl) $ throwError $ VerifierError $
        "gas exhausted in verifier. attempted to charge " <> sshow (case g of Gas g' -> g') <>
        " with only " <> sshow (case gl of GasLimit gl' -> gl') <> " remaining."
    lift $ writeSTRef r (gl - fromIntegral g)

runVerifierPlugins :: Map Text VerifierPlugin -> GasLimit -> Command (Payload PublicMeta ParsedCode) -> IO (Either VerifierError GasLimit)
runVerifierPlugins allVerifiers gl tx = try $ stToIO $ do
    gasRef <- newSTRef gl
    either throw (\_ -> readSTRef gasRef) <=< runExceptT $ Merge.mergeA
        (Merge.traverseMissing $ \k _ -> throwError $ VerifierError ("verifier does not exist: " <> k))
        Merge.dropMissing
        (Merge.zipWithAMatched $ \vn argsAndCaps verifierPlugin ->
            for_ argsAndCaps $ \(proof, caps) -> do
                verifierGasLimit <- lift $ readSTRef gasRef
                runVerifierPlugin verifierPlugin proof caps gasRef
                verifierDoneGasLimit <- lift $ readSTRef gasRef
                if verifierDoneGasLimit > verifierGasLimit
                then throwError $ VerifierError ("Verifier attempted to charge negative gas: " <> vn)
                else return ()
            )
        usedVerifiers
        allVerifiers
    where
    usedVerifiers :: Map Text [(PactValue, Set SigCapability)]
    usedVerifiers =
        Map.fromListWith (++) $
        fmap
            (\Verifier {_verifierName = VerifierName name, _verifierProof = ParsedVerifierProof proof, _verifierCaps = caps} ->
                (name, [(proof, Set.fromList caps)])) $
        fromMaybe [] $ _pVerifiers (_cmdPayload tx)
