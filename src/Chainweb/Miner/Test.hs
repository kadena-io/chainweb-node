{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.Miner.Test
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--

module Chainweb.Miner.Test
( MinerConfig(..)
, configMeanBlockTimeSeconds
, configWindowWidth
, defaultMinerConfig
, pMinerConfig
, miner
) where

import Configuration.Utils

import Control.Concurrent
import Control.Lens hiding ((.=))
import Control.Monad (when)
import Control.Monad.STM

import Data.IORef
import Data.Reflection hiding (int)
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..))
import Data.Word (Word64)

import GHC.Generics (Generic)

import Numeric.Natural

import System.LogLevel
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB (BlockHeaderDb)
import Chainweb.ChainId (ChainId)
import Chainweb.Cut
import Chainweb.CutDB
import Chainweb.Difficulty (HashTarget(..))
import Chainweb.Graph
import Chainweb.NodeId
import Chainweb.Time (getCurrentTimeIntegral)
import Chainweb.TreeDB.HashTarget (BlockRate(..), WindowWidth(..), hashTarget)
import Chainweb.Utils
import Chainweb.Version (usePOW)
import Chainweb.WebBlockHeaderDB

import Data.DiGraph
import Data.LogMessage

-- -------------------------------------------------------------------------- --
-- Configuration of Example

data MinerConfig = MinerConfig
    { _configMeanBlockTimeSeconds :: !Natural
    , _configWindowWidth :: !Natural
    }
    deriving (Show, Eq, Ord, Generic)

makeLenses ''MinerConfig

defaultMinerConfig :: MinerConfig
defaultMinerConfig = MinerConfig
    { _configMeanBlockTimeSeconds = 10
    , _configWindowWidth = 5
    }

instance ToJSON MinerConfig where
    toJSON o = object
        [ "meanBlockTimeSeconds" .= _configMeanBlockTimeSeconds o
        , "windowWidth" .= _configWindowWidth o
        ]

instance FromJSON (MinerConfig -> MinerConfig) where
    parseJSON = withObject "MinerConfig" $ \o -> id
        <$< configMeanBlockTimeSeconds ..: "meanBlockTimeSeconds" % o
        <*< configWindowWidth ..: "windowWidth" % o

pMinerConfig :: MParser MinerConfig
pMinerConfig = id
    <$< configMeanBlockTimeSeconds .:: option auto
        % long "mean-block-time"
        <> short 'b'
        <> help "mean time for mining a block seconds"
    <*< configWindowWidth .:: option auto
        % long "window-width"
        <> short 'w'
        <> help "the number of blocks to consider as an 'epoch window' during POW mining"

-- -------------------------------------------------------------------------- --
-- Miner

miner
    :: LogFunction
    -> MinerConfig
    -> NodeId
    -> CutDb
    -> WebBlockHeaderDb
    -> IO ()
miner logFun conf nid cutDb wcdb = do
    logg Info "Started Miner"
    gen <- MWC.createSystemRandom
    give wcdb $ go gen 1
  where
    logg :: LogLevel -> T.Text -> IO ()
    logg = logFun

    graph :: ChainGraph
    graph = _chainGraph cutDb

    go :: Given WebBlockHeaderDb => MWC.GenIO -> Int -> IO ()
    go gen !i = do
        nonce0 <- MWC.uniform gen
        counter <- newIORef (1 :: Int)
        go' gen i nonce0 counter

    go' :: Given WebBlockHeaderDb => MWC.GenIO -> Int -> Word64 -> IORef Int -> IO ()
    go' gen !i !nonce0 counter = do

        -- Create a new (test) block header.
        --
        let mine :: Word64 -> IO (Either Word64 Cut)
            mine !nonce = do
                -- Get the current longest cut.
                --
                c <- _cut cutDb

                -- Randomly pick a chain to mine on.
                --
                cid <- randomChainId c

                -- The parent block the mine on. Any given chain will always
                -- contain at least a genesis block, so this otherwise naughty
                -- `^?!` will always succeed.
                --
                let !p = c ^?! ixg cid

                -- The hashing target to be lower than.
                --
                target <- getTarget cid p

                -- Artificially delay the mining process when not using
                -- proof-of-work mining.
                --
                when (not . usePOW $ _blockChainwebVersion p) $ do
                    d <- MWC.geometric1
                         (int (order graph) / (int (_configMeanBlockTimeSeconds conf) * 1000000))
                         gen
                    threadDelay d

                -- The new block's creation time. Must come after any simulated
                -- delay.
                --
                ct <- getCurrentTimeIntegral

                -- let targetBits :: String
                --     targetBits = printf "%0256b" $ htInteger target

                -- Loops (i.e. "mines") if a non-matching nonce was generated.
                --
                -- INVARIANT: `testMine` will succeed on the first attempt when
                -- POW is not used.
                --
                testMine (Nonce nonce) target ct nid cid c >>= \case
                    Left BadNonce -> do
                        -- atomicModifyIORef' counter (\n -> (succ n, ()))
                        mine $! succ nonce
                    Left BadAdjacents ->
                        pure $! Left nonce
                    Right (T2 _ {- newBh -} newCut) -> do

                        -- DEBUGGING --
                        -- Uncomment the following for a live view of mining
                        -- results on Chain 0. You will have to uncomment a
                        -- number of surrounding helper values and readd some
                        -- imports.

                        -- total <- readIORef counter
                        -- when (cid == testChainId 0) $ do
                        --   printf "\n--- NODE:%02d success! HASHES:%06x TARGET:%s...%s PARENT-H:%03x PARENT-W:%06x PARENT:%s NEW:%s\n"
                        --       (_nodeIdId nid)
                        --       total
                        --       (take 30 targetBits)
                        --       (drop 226 targetBits)
                        --       (pheight p)
                        --       (pweight p)
                        --       (take 8 . drop 5 . show $ _blockHash p)
                        --       (take 8 . drop 5 . show $ _blockHash newBh)

                        pure $! Right newCut

        mine nonce0 >>= \case
          Left nonce -> go' gen i nonce counter
          Right c' -> do
              logg Info $! "created new block" <> sshow i

              -- Publish the new Cut into the CutDb (add to queue).
              --
              atomically $! addCutHashes cutDb (cutToCutHashes Nothing c')

              go gen (i + 1)

    getTarget :: ChainId -> BlockHeader -> IO HashTarget
    getTarget cid bh
        | not $ usePOW (_blockChainwebVersion bh) = pure $! _blockTarget bh
        | otherwise = case blockDb cid of
              Nothing -> pure $! _blockTarget bh
              Just db -> hashTarget db bh
                             (BlockRate $ _configMeanBlockTimeSeconds conf)
                             (WindowWidth $ _configWindowWidth conf)

    blockDb :: ChainId -> Maybe BlockHeaderDb
    blockDb cid = wcdb ^? webBlockHeaderDb . ix cid

    -- htInteger :: HashTarget -> Integer
    -- htInteger (HashTarget (PowHashNat w)) = fromIntegral w

    -- pheight :: BlockHeader -> Word64
    -- pheight bh = case _blockHeight bh of BlockHeight w -> w

    -- pweight :: BlockHeader -> Integer
    -- pweight bh = case _blockWeight bh of
    --     BlockWeight (HashDifficulty (PowHashNat w)) -> int w
