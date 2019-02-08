{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: ChainDb2Gexf
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

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM

import qualified Streaming.Prelude as S

import System.Environment
import System.Path

-- internal modules

import Chainweb.BlockHeader
import Chainweb.TreeDB.Persist

import Utils.Gexf

db2gexf
    :: MonadUnliftIO m
    => MonadThrow m
    => Foldable f
    => f (Path Absolute)
    -> Path Absolute
    -> m ()
db2gexf inPaths outPath = runResourceT $ do
    headers <- S.fold_ (\m a -> HM.insert (_blockHash a) a m) mempty id
        $ flip S.for fileEntries
        $ S.each inPaths
    liftIO $ B.writeFile (toFilePath outPath) $ blockMap2gexf headers

main :: IO ()
main = do
    outFile : inFiles <- getArgs >>= mapM (makeAbsolute . fromFilePath)
    db2gexf inFiles outFile

