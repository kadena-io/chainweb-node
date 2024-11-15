{-# language
    ImportQualifiedPost
#-}

module Main (main) where

import Chainweb.Pact.Backend.Compaction qualified as Compact

main :: IO ()
main = do
  Compact.main
