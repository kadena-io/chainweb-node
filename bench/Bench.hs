module Main where

import Criterion.Main

import qualified Chainweb.Pact.Backend.Bench as Checkpointer

main :: IO ()
main = do
  defaultMain
    [ Checkpointer.bench ]
