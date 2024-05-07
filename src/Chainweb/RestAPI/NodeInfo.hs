{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Network.HTTP.Types
import Network.Wai

import GHC.Generics

import Web.DeepRoute
import Web.DeepRoute.Wai

import Servant hiding (respond)

import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Graph
import Chainweb.RestAPI.Utils
import Chainweb.Utils.Rule
import Chainweb.Version

nodeInfoApi :: ChainwebVersion -> CutDb cas -> Route Application
nodeInfoApi v cutDb =
  seg "info" $ endpoint methodGet ("application/json") $ \_ respond -> do
    curCut <- liftIO $ _cut cutDb
    let ch = cutToCutHashes Nothing curCut
        curHeight = maximum $ map _bhwhHeight $ HashMap.elems $ _cutHashes ch
        graphs = unpackGraphs v
        curGraph = head $ dropWhile (\(h,_) -> h > curHeight) graphs
        curChains = map fst $ snd curGraph
    respond $ responseJSON status200 [] $ NodeInfo
      { nodeVersion = _versionName v
      , nodeApiVersion = prettyApiVersion
      , nodeChains = T.pack . show <$> curChains
      , nodeNumberOfChains = length curChains
      , nodeGraphHistory = graphs
      , nodeLatestBehaviorHeight = latestBehaviorAt v
      }

type NodeInfoApi = "info" :> Get '[JSON] NodeInfo

someNodeInfoApi :: SomeApi
someNodeInfoApi = SomeApi (Proxy @NodeInfoApi)

data NodeInfo = NodeInfo
  {
    nodeVersion :: ChainwebVersionName
  , nodeApiVersion :: Text
  , nodeChains :: [Text]
  -- ^ Current list of chains
  , nodeNumberOfChains :: !Int
  -- ^ Current number of chains
  , nodeGraphHistory :: [(BlockHeight, [(Int, [Int])])]
  -- ^ List of chain graphs and the block height they took effect. Sorted
  -- descending by height so the current chain graph is at the beginning.
  , nodeLatestBehaviorHeight :: BlockHeight
  -- ^ Height at which the latest behavior of the node is activated. See
  -- `Chainweb.Version.latestBehaviorAt`.
  } deriving (Show, Eq, Generic)

instance ToJSON NodeInfo
instance FromJSON NodeInfo

-- | Converts chainwebGraphs to a simpler structure that has invertible JSON
-- instances.
unpackGraphs :: ChainwebVersion -> [(BlockHeight, [(Int, [Int])])]
unpackGraphs v = gs
  where
    gs = map (second graphAdjacencies) $ NE.toList $ ruleElems (BlockHeight 0) $ _versionGraphs v
    graphAdjacencies = map unChain . HashMap.toList . fmap HashSet.toList . G.adjacencySets . view chainGraphGraph
    unChain (a, bs) = (chainIdInt a, map chainIdInt bs)
