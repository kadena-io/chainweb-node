{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
module Chainweb.VerifierPlugin.Allow(plugin) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.ST
import Control.Monad.Trans.Class
import Data.Aeson
import qualified Data.Set as Set
import Data.STRef
import qualified Data.Text.Encoding as Text

import Pact.Types.Capability
import Pact.Types.Exp
import Pact.Types.PactValue

import Chainweb.VerifierPlugin

-- This trivial verifier plugin takes as arguments a list of JSON-encoded
-- capabilities, and grants any subset of them.
plugin :: VerifierPlugin
plugin = VerifierPlugin $ \args caps gl ->
    runST $ runExceptT $ do
        gasRef <- lift $ newSTRef gl
        chargeGas gasRef 100
        decodedArgs :: [UserCapability] <-
            traverse decodeArgToCap args
        unless (noDuplicates decodedArgs) $
            throwError $ VerifierError "duplicate capabilities exist in the arguments"
        unless (caps `Set.isSubsetOf` Set.fromList decodedArgs) $
            throwError $ VerifierError "granted capabilities are not a subset of those in the arguments"
        lift $ readSTRef gasRef
    where
    noDuplicates :: Ord a => [a] -> Bool
    noDuplicates = go Set.empty
        where
        go _ [] = True
        go seen (x:xs) =
            not (Set.member x seen) &&
            (let !seen' = Set.insert x seen in go seen' xs)
    decodeArgToCap (PLiteral (LString arg)) =
        case eitherDecodeStrict' (Text.encodeUtf8 arg) of
            Left _err -> throwError $ VerifierError $ "argument was not a JSON-encoded capability: " <> arg
            Right cap -> return cap
    decodeArgToCap _ =
        throwError $ VerifierError "expected string literal in verifier arguments"
