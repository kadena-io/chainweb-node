{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | An endpoint for getting node information.

module Chainweb.RestAPI.NodeInfo where

import Control.Lens
import Control.Monad.Trans
import Data.Aeson
import Data.Bifunctor
import qualified Data.DiGraph as G
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Generics

import Servant

import Chainweb.BlockHeight
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.RestAPI.Utils
import Chainweb.Version

type NodeInfoApi = "info" :> Get '[JSON] NodeInfo

someNodeInfoApi :: SomeApi
someNodeInfoApi = SomeApi (Proxy @NodeInfoApi)

someNodeInfoServer :: ChainwebVersion -> CutDb cas -> SomeServer
someNodeInfoServer v c =
  SomeServer (Proxy @NodeInfoApi) (nodeInfoHandler v $ someCutDbVal v c)

data NodeInfo = NodeInfo
  {
    nodeVersion :: ChainwebVersion
  , nodeApiVersion :: Text
  , nodeChains :: [Text]
  -- ^ Current list of chains
  , nodeNumberOfChains :: !Int
  -- ^ Current number of chains
  , nodeGraphHistory :: [(BlockHeight, [(Int, [Int])])]
  -- ^ List of chain graphs and the block height they took effect. Sorted
  -- descending by height so the current chain graph is at the beginning.
  } deriving (Show, Eq, Generic)

instance ToJSON NodeInfo
instance FromJSON NodeInfo

nodeInfoHandler :: ChainwebVersion -> SomeCutDb cas -> Server NodeInfoApi
nodeInfoHandler v (SomeCutDb ((CutDbT db) :: CutDbT cas v)) = do
    curCut <- liftIO $ _cut db
    let ch = cutToCutHashes Nothing curCut
        curHeight = maximum $ map _bhwhHeight $ HashMap.elems $ _cutHashes ch
        graphs = unpackGraphs v
        curGraph = head $ dropWhile (\(h,_) -> h > curHeight) graphs
        curChains = map fst $ snd curGraph
    return $ NodeInfo
      { nodeVersion = v
      , nodeApiVersion = prettyApiVersion
      , nodeChains = T.pack . show <$> curChains
      , nodeNumberOfChains = length curChains
      , nodeGraphHistory = graphs
      }

-- | Converts chainwebGraphs to a simpler structure that has invertible JSON
-- instances.
unpackGraphs :: ChainwebVersion -> [(BlockHeight, [(Int, [Int])])]
unpackGraphs v = gs
  where
    gs = map (second graphAdjacencies) $ NE.toList $ chainwebGraphs v
    graphAdjacencies = map unChain . HashMap.toList . fmap HashSet.toList . G.adjacencySets . view chainGraphGraph
    unChain (a, bs) = (chainIdInt a, map chainIdInt bs)

