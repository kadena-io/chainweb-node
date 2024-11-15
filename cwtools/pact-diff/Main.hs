{-# language
    ImportQualifiedPost
#-}

module Main (main) where

import Chainweb.Pact.Backend.PactState.Diff qualified as PactDiff

main :: IO ()
main = do
  PactDiff.main
