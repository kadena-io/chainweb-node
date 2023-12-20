{-# language DerivingStrategies #-}
{-# language EmptyCase #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language TypeApplications #-}

module Chainweb.VerifierPlugin
    ( VerifierPlugin(..)
    , VerifierError(..)
    , runVerifierPlugins
    , ShouldRunVerifierPlugins(..)
    ) where

import Control.DeepSeq
import Control.Exception.Safe(Exception, throwIO)
import Control.Monad

import Data.Foldable
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Merge
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Text(Text)

import Chainweb.Transaction

import Pact.Types.Capability
import Pact.Types.Command
import Pact.Types.PactValue
import Pact.Types.Verifier

data VerifierError = VerifierError Text
    deriving stock Show
instance Exception VerifierError

data ShouldRunVerifierPlugins = RunVerifierPlugins | DoNotRunVerifierPlugins

newtype VerifierPlugin
    = VerifierPlugin
    { runVerifierPlugin :: ChainwebTransaction -> [PactValue] -> Set SigCapability -> IO ()
    }
    deriving newtype NFData

runVerifierPlugins :: Map Text VerifierPlugin -> ChainwebTransaction -> IO ()
runVerifierPlugins allVerifiers tx =
    void $ Merge.mergeA
        (Merge.traverseMissing $ \k _ -> throwIO $ VerifierError ("verifier does not exist: " <> k))
        Merge.dropMissing
        (Merge.zipWithAMatched $ \_vn argsAndCaps verifierPlugin ->
            for_ argsAndCaps $ \(args, caps) ->
                runVerifierPlugin verifierPlugin tx args caps
            )
        usedVerifiers
        allVerifiers
    where
    usedVerifiers :: Map Text [([PactValue], Set SigCapability)]
    usedVerifiers =
        Map.fromListWith (++) $
        fmap
            (\Verifier {_verifierName = VerifierName name, _verifierArgs = ParsedVerifierArgs args, _verifierCaps = caps} ->
                (name, [(args, Set.fromList caps)])) $
        fromMaybe [] $ _pVerifiers (payloadObj $ _cmdPayload tx)
