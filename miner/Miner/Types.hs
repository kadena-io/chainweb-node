{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Miner.Types where

import Data.DoubleWord (Word128, Word256)
import Data.Swagger (NamedSchema(..))
import Data.Swagger.Schema (ToSchema(..))

-- internal modules
import Chainweb.BlockHash (BlockHashRecord)
import Chainweb.BlockHeader
import Chainweb.Difficulty
import Chainweb.MerkleLogHash (MerkleLogHash)
import Chainweb.NodeId (ChainNodeId)
import Chainweb.RestAPI.Orphans ()
import Chainweb.Time (Micros, Time, TimeSpan)
import Chainweb.Version (ChainwebVersion)

---

deriving instance ToSchema BlockCreationTime
deriving instance ToSchema BlockHashRecord
deriving instance ToSchema BlockHeight
deriving instance ToSchema BlockWeight
deriving instance ToSchema ChainNodeId
deriving instance ToSchema HashDifficulty
deriving instance ToSchema HashTarget
deriving instance ToSchema Micros
deriving instance ToSchema Nonce
deriving instance ToSchema PowHashNat
deriving instance ToSchema Word128
deriving instance ToSchema Word256
deriving instance ToSchema a => ToSchema (Time a)
deriving instance ToSchema a => ToSchema (TimeSpan a)

instance ToSchema ChainwebVersion where
  declareNamedSchema _ = pure $ NamedSchema (Just "ChainwebVersion") mempty

instance ToSchema MerkleLogHash where
  declareNamedSchema _ = pure $ NamedSchema (Just "MerkleLogHash") mempty
