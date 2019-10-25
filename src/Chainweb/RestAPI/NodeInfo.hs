{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | An endpoint for getting node information.

module Chainweb.RestAPI.NodeInfo where

import Data.Aeson
import Data.Foldable
import qualified Data.HashSet as HashSet
import Data.Text (Text)
import Data.Swagger.Schema

import GHC.Generics

import Servant

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
  , nodeNumberOfChains :: !Int
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
  }

deriving instance ToSchema NodeInfo
