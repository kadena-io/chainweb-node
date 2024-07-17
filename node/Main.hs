{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import ChainwebNode qualified

main :: IO ()
main = do
  ChainwebNode.main
