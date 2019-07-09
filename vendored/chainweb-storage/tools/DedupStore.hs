{-# LANGUAGE BangPatterns #-}

-- |
-- Module: DedupStore
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Main
( main
) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Random as BR

-- internal modules

import Data.CAS.Forgetful
import Data.DedupStore

-- -------------------------------------------------------------------------- --
-- Simple Test

main :: IO ()
main = do
    !bs <- BR.random (1024*1024*200)
    k <- dedupStore ForgetfulCas $! BL.fromStrict bs
    print k

