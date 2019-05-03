{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Test.CutDB.Test
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.CutDB.Test
( test
) where

import Chainweb.Graph
import Chainweb.Test.CutDB
import Chainweb.Test.Pact.Utils
import Chainweb.Version

import Data.CAS.RocksDB
import Data.LogMessage
import qualified Data.Text.IO as T

import System.LogLevel

test :: IO ()
test = do
    -- Pact service that is used to initialize the cut data base
    pactExec <- testWebPactExecutionService v txGenerator
    withTempRocksDb "chainweb-sbv-tests"  $ \rdb ->
        withTestCutDb rdb v 20 pactExec logg $ \cutDb -> do

            -- pact service, that is used to extend the cut data base
            pactExec2 <- testWebPactExecutionService v txGenerator
            syncPact cutDb pactExec2
            extendTestCutDb cutDb pactExec2 20
  where
    v = TimedCPM petersonChainGraph
    txGenerator _cid _bockHeight _blockHash = return mempty {- Vector of ChainwebTransaction -}
    logg l
        | l <= Warn = T.putStrLn . logText
        | otherwise = const $ return ()

