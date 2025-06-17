{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}

-- | An endpoint for getting node information.

module Chainweb.RestAPI.NodeInfo where

import Control.Lens hiding ((.=))
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
import Chainweb.Ranked
import Chainweb.RestAPI.Utils
import Chainweb.Utils
import Chainweb.Utils.Rule
import Chainweb.Version

type NodeInfoApi = "info" :> Get '[JSON] NodeInfo

someNodeInfoApi :: SomeApi
someNodeInfoApi = SomeApi (Proxy @NodeInfoApi)

someNodeInfoServer :: HasVersion => CutDb -> SomeServer
someNodeInfoServer c =
  SomeServer (Proxy @NodeInfoApi) (nodeInfoHandler $ someCutDbVal c)

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
  , nodeServiceDate :: Maybe Text
    -- ^ The upcoming service date for the node.
  , nodeBlockDelay :: BlockDelay
    -- ^ The PoW block delay of the node (microseconds)
  , nodePayloadProviders :: ChainMap Value
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

nodeInfoHandler :: HasVersion => SomeCutDb -> Server NodeInfoApi
nodeInfoHandler (SomeCutDb (CutDbT db :: CutDbT v)) = do
    curCut <- liftIO $ _cut db
    let ch = cutToCutHashes Nothing curCut
    let curHeight = maximum $ map _rankedHeight $ HM.elems $ _cutHashes ch
    let graphs = unpackGraphs
    let curGraph = unsafeHead "Chainweb.RestAPI.NodeInfo.nodeInfoHandler.curGraph" $ dropWhile (\(h,_) -> h > curHeight) graphs
    let curChains = map fst $ snd curGraph
    return $ NodeInfo
      { nodeVersion = _versionName implicitVersion
      , nodePackageVersion = chainwebNodeVersionHeaderValue
      , nodeApiVersion = prettyApiVersion
      , nodeChains = T.pack . show <$> curChains
      , nodeNumberOfChains = length curChains
      , nodeGraphHistory = graphs
      , nodeLatestBehaviorHeight = latestBehaviorAt
      , nodeGenesisHeights = map (\c -> (chainIdToText c, genesisHeight c)) $ HS.toList chainIds
      , nodeHistoricalChains =
        ruleElems $ fmap (HM.toList . HM.map HS.toList . toAdjacencySets) $ _versionGraphs implicitVersion
      , nodeServiceDate = T.pack <$> _versionServiceDate implicitVersion
      , nodeBlockDelay = _versionBlockDelay implicitVersion
      , nodePayloadProviders = _versionPayloadProviderTypes implicitVersion <&> \case
          EvmProvider n -> object
            [ "type" .= ("eth" :: Text)
            , "ethChainId" .= n
            ]
          PactProvider -> object
            [ "type" .= ("pact" :: Text)
            ]
          MinimalProvider -> object
            [ "type" .= ("parked" :: Text)
            ]
      }

-- | Converts chainwebGraphs to a simpler structure that has invertible JSON
-- instances.
unpackGraphs :: HasVersion => [(BlockHeight, [(Int, [Int])])]
unpackGraphs = gs
  where
    gs = map (second graphAdjacencies) $ NE.toList $ ruleElems $ _versionGraphs implicitVersion
    graphAdjacencies = map unChain . HM.toList . fmap HS.toList . G.adjacencySets . view chainGraphGraph
    unChain (a, bs) = (chainIdInt a, map chainIdInt bs)
