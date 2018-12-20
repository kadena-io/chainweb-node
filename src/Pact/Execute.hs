module Pact.Execute where

import Pact.Repl

main :: IO ()
main = do
  _ <- repl
  return ()
