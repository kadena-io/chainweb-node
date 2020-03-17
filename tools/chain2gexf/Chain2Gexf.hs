{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: ChainDb2Gexf
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chain2Gexf ( main, opts ) where

import Control.Error.Util (note)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Options.Applicative

import qualified Streaming.Prelude as S

import System.Path

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..))
import Chainweb.Graph (petersonChainGraph)
import Chainweb.TreeDB.Persist (fileEntries)
import Chainweb.Version
    (ChainwebVersion(..), chainwebVersionFromText, chainwebVersionId)

import Gexf

---

data Env = Env ChainwebVersion FilePath [FilePath]

pEnv :: Parser Env
pEnv = Env <$> version <*> inFile <*> outFiles
  where
    cver :: ReadM ChainwebVersion
    cver = eitherReader $ \s ->
        note "Illegal ChainwebVersion" . chainwebVersionFromText $ T.pack s

    version :: Parser ChainwebVersion
    version = option cver (long "version" <> metavar "VERSION"
                           <> value (TimedConsensus petersonChainGraph)
                           <> help "Chainweb Version used to produce the INPUT FILES")

    inFile :: Parser FilePath
    inFile = strOption (long "output" <> metavar "FILE" <> value "out.gexf" <> help "Output .gexf file")

    outFiles :: Parser [FilePath]
    outFiles = some (argument str (metavar "INPUT-FILES"))

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

opts :: ParserInfo Env
opts = info (pEnv <**> helper)
    (fullDesc
     <> progDesc theDesc
     <> header ("chain2gexf - " <> theDesc))
  where
    theDesc = "Convert a persisted Chainweb into .gexf format for viewing"
