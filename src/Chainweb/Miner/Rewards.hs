{-# LANGUAGE TemplateHaskell #-}

module Chainweb.Miner.Rewards where

import Data.ByteString (ByteString)
import Data.FileEmbed

rawMinerRewards :: ByteString
rawMinerRewards = $(embedFile "rewards/miner_rewards.csv")
