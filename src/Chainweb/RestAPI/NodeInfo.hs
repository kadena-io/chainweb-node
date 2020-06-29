{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | An endpoint for getting node information.

module Chainweb.RestAPI.NodeInfo where

import Control.Lens
import Data.Aeson
import Data.Bifunctor
import qualified Data.DiGraph as G
import Data.Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Swagger.Schema

import GHC.Generics

import Servant

import Chainweb.BlockHeight
import Chainweb.Graph
import Chainweb.RestAPI.Utils
import Chainweb.Version

type NodeInfoApi = "info" :> Get '[JSON] NodeInfo

someNodeInfoApi :: SomeApi
someNodeInfoApi = SomeApi (Proxy @NodeInfoApi)

someNodeInfoServer :: ChainwebVersion -> SomeServer
someNodeInfoServer v =
  SomeServer (Proxy @NodeInfoApi) (nodeInfoHandler v)

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
  } deriving (Generic)

instance ToJSON NodeInfo
instance FromJSON NodeInfo

nodeInfoHandler :: ChainwebVersion -> Server NodeInfoApi
nodeInfoHandler v = return
  NodeInfo
  { nodeVersion = v
  , nodeApiVersion = prettyApiVersion
  , nodeChains = (chainIdToText <$> (toList $ chainIds v))
  , nodeNumberOfChains = HashSet.size $ chainIds v
  , nodeGraphHistory = unpackGraphs v
  }

deriving instance ToSchema NodeInfo


-- | Converts chainwebGraphs to a simpler structure that has invertible JSON
-- instances.
unpackGraphs :: ChainwebVersion -> [(BlockHeight, [(Int, [Int])])]
unpackGraphs v = gs
  where
    gs = map (second graphAdjacencies) $ NE.toList $ chainwebGraphs v
    graphAdjacencies = map unChain . HashMap.toList . fmap HashSet.toList . G.adjacencySets . view chainGraphGraph
    unChain (a, bs) = (chainIdInt a, map chainIdInt bs)
