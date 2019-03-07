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
module Main ( main ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Maybe (fromJust)

import Options.Applicative

import qualified Streaming.Prelude as S

import System.Path

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.TreeDB.Persist (fileEntries)
import Chainweb.Version (ChainwebVersion, chainwebVersionId, chainwebVersionFromText)

import Utils.Gexf

---

data Env = Env ChainwebVersion FilePath [FilePath]

pEnv :: Parser Env
pEnv = Env
    <$> (f <$> strOption (long "version" <> metavar "VERSION" <> value "testWithTime"
                   <> help "Chainweb Version used to produce the INPUT FILES"))
    <*> strOption (long "output" <> metavar "FILE" <> value "out.gexf" <> help "Output .gexf file")
    <*> some (argument str (metavar "INPUT-FILES"))
  where
    f :: String -> ChainwebVersion
    f = fromJust . chainwebVersionFromText . T.pack

db2gexf :: [Path Absolute] -> Path Absolute -> IO ()
db2gexf inPaths outPath = runResourceT $ do
    headers <- S.fold_ (\m a -> HM.insert (_blockHash a) a m) mempty id
        $ flip S.for fileEntries
        $ S.each inPaths
    liftIO $ B.writeFile (toFilePath outPath) $ blockMap2gexf headers

main :: IO ()
main = do
    Env v o0 i0 <- execParser opts
    let !w = chainwebVersionId v
    i <- traverse (makeAbsolute . fromFilePath) i0
    o <- makeAbsolute $ fromFilePath o0
    putStrLn $ "Generating GEXF file for: " <> show w
    db2gexf i o
  where
    opts = info (pEnv <**> helper)
        (fullDesc
         <> header "chain2gexf - Convert a persisted Chainweb into .gexf format for viewing.")
