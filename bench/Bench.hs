module Main where

import Criterion.Main

import qualified Chainweb.Pact.Backend.Bench as Checkpointer
import qualified Chainweb.Pact.Backend.ForkingBench as ForkingBench

main :: IO ()
main = do
  defaultMain
    [
      Checkpointer.bench ,
      ForkingBench.bench
    ]
