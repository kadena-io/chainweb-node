{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
module Chainweb.VerifierPlugin.Trivial(plugin) where

import Control.Lens
import Control.Monad
import Data.Aeson
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Pact.Types.Capability
import Pact.Types.Exp
import Pact.Types.Gas
import Pact.Types.PactValue

import Chainweb.VerifierPlugin

-- This trivial verifier plugin takes as arguments a list of JSON-encoded
-- capabilities, and grants any subset of them.
plugin :: VerifierPlugin
plugin = VerifierPlugin $ \args caps gl -> over _Left VerifierError $ do
    decodedArgs :: [UserCapability] <- traverse decodeArgToCap args
    unless (noDuplicates decodedArgs) $
        Left "duplicate capabilities exist in the arguments"
    unless (caps `Set.isSubsetOf` Set.fromList decodedArgs) $
        Left "granted capabilities are not a subset of those in the arguments"
    return (Gas 20)
    where
    noDuplicates :: Ord a => [a] -> Bool
    noDuplicates = go Set.empty
        where
        go _ [] = True
        go seen (x:xs) =
            not (Set.member x seen) &&
            (let !seen' = Set.insert x seen in go seen' xs)
    decodeArgToCap (PLiteral (LString arg)) =
        over _Left Text.pack $ eitherDecodeStrict' (Text.encodeUtf8 arg)
    decodeArgToCap _ =
        Left "expected string literal in verifier arguments"
