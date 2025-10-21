{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | An endpoint for getting node information.

module Chainweb.RestAPI.NodeInfo where

import Control.Lens
import Control.Monad.Trans
import Data.Aeson
import Data.Bifunctor
import Data.HashSet qualified as HS
import qualified Data.DiGraph as G
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Generics

import Servant

import Chainweb.BlockHeader (genesisHeight)
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Difficulty (BlockDelay)
import Chainweb.Graph
import Chainweb.RestAPI.Utils
import Chainweb.Utils
import Chainweb.Utils.Rule
import Chainweb.Version

type NodeInfoApi = "info" :> Get '[JSON] NodeInfo

someNodeInfoApi :: SomeApi
someNodeInfoApi = SomeApi (Proxy @NodeInfoApi)

someNodeInfoServer :: ChainwebVersion -> CutDb tbl -> SomeServer
someNodeInfoServer v c =
  SomeServer (Proxy @NodeInfoApi) (nodeInfoHandler v $ someCutDbVal v c)

data NodeInfo = NodeInfo
  { nodeVersion :: ChainwebVersionName
    -- ^ ChainwebVersion the node is running
  , nodePackageVersion :: Text
    -- ^ Chainweb Package version that the node is running
  , nodeApiVersion :: Text
    -- ^ Chainweb Node API version
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
  , nodeGenesisHeights :: [(Text, BlockHeight)]
    -- ^ Genesis heights of each chain.
  , nodeHistoricalChains :: NE.NonEmpty (BlockHeight, [(ChainId, [ChainId])])
    -- ^ All graph upgrades
  , nodeBlockDelay :: BlockDelay
    -- ^ The PoW block delay of the node (microseconds)
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

nodeInfoHandler :: ChainwebVersion -> SomeCutDb tbl -> Server NodeInfoApi
nodeInfoHandler v (SomeCutDb (CutDbT db :: CutDbT cas v)) = do
    curCut <- liftIO $ _cut db
    let ch = cutToCutHashes Nothing curCut
    let curHeight = maximum $ map _bhwhHeight $ HM.elems $ _cutHashes ch
    let graphs = unpackGraphs v
    let curGraph = unsafeHead "Chainweb.RestAPI.NodeInfo.nodeInfoHandler.curGraph" $ dropWhile (\(h,_) -> h > curHeight) graphs
    let curChains = map fst $ snd curGraph
    return $ NodeInfo
      { nodeVersion = _versionName v
      , nodePackageVersion = chainwebNodeVersionHeaderValue
      , nodeApiVersion = prettyApiVersion
      , nodeChains = T.pack . show <$> curChains
      , nodeNumberOfChains = length curChains
      , nodeGraphHistory = graphs
      , nodeLatestBehaviorHeight = latestBehaviorAt v
      , nodeGenesisHeights = map (\c -> (chainIdToText c, genesisHeight v c)) $ HS.toList (chainIds v)
      , nodeHistoricalChains = ruleElems $ fmap (HM.toList . HM.map HS.toList . toAdjacencySets) $ _versionGraphs v
      , nodeBlockDelay = _versionBlockDelay v
      }

-- | Converts chainwebGraphs to a simpler structure that has invertible JSON
-- instances.
unpackGraphs :: ChainwebVersion -> [(BlockHeight, [(Int, [Int])])]
unpackGraphs v = gs
  where
    gs = map (second graphAdjacencies) $ NE.toList $ ruleElems $ _versionGraphs v
    graphAdjacencies = map unChain . HM.toList . fmap HS.toList . G.adjacencySets . view chainGraphGraph
    unChain (a, bs) = (chainIdInt a, map chainIdInt bs)
