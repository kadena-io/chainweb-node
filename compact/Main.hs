{-# language
    ImportQualifiedPost
#-}

module Main (main) where

import Chainweb.Pact.Backend.CompactionInMemory qualified as Compact

main :: IO ()
main = do
  Compact.main
