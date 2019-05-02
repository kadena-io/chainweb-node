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

import Data.LogMessage
import qualified Data.Text.IO as T
import Chainweb.Test.CutDB
import Chainweb.Test.Pact.Utils
import Chainweb.Version
import Chainweb.Graph
import Data.CAS.RocksDB

test :: IO ()
test = do
    pactExec <- testWebPactExecutionService v txGenerator
    withTempRocksDb "chainweb-sbv-tests"  $ \rdb ->
        withTestCutDb rdb v 20 pactExec logg $ \cutDb ->
            return ()
  where
    v = TimedCPM petersonChainGraph
    txGenerator cid bockHeight blockHash = return mempty {- Vector of ChainwebTransaction -}
    logg _ = T.putStrLn . logText
