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
import Control.Monad (unless)
import Control.Monad.STM

import qualified Data.HashMap.Strict as HM
import Data.Reflection hiding (int)
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..), T3(..))
import Data.Word (Word64)

import GHC.Generics (Generic)

import Numeric.Natural

import System.LogLevel
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

-- internal modules

import Chainweb.BlockHash (BlockHash)
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

-- DEBUGGING --
-- import Chainweb.ChainId (testChainId)
-- import Chainweb.Difficulty (PowHashNat(..), HashDifficulty(..))
-- import Chainweb.Time (Time(..), TimeSpan(..))
-- import Data.Generics.Wrapped (_Unwrapped)
-- import Data.IORef
-- import Data.Int (Int64)
-- import System.IO (hFlush, stdout)
-- import Text.Printf (printf)

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

type Adjustments = HM.HashMap BlockHash (T2 BlockHeight HashTarget)

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
    give wcdb $ go gen 1 HM.empty
  where
    logg :: LogLevel -> T.Text -> IO ()
    logg = logFun

    graph :: ChainGraph
    graph = _chainGraph cutDb

    go :: Given WebBlockHeaderDb
       => MWC.GenIO
       -> Int
       -> Adjustments
       -> IO ()
    go gen !i !adjustments0 = do

        nonce0 <- MWC.uniform gen

        -- counter <- newIORef (1 :: Int)

        -- Create a new (test) block header.
        --
        -- INVARIANT: A new cut, chain, and parent header is reselected after
        -- each hash failure. This ensures that the Cut the miner is working on
        -- doesn't grow stale, and cause forks. Without this condition (or a
        -- similar one), the miners cause forks quite aggressively.
        --
        let mine
                :: Word64
                -> Adjustments
                -> IO (T3 BlockHeader Cut Adjustments)
            mine !nonce !adjustments = do
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
                T2 target adjustments' <- getTarget cid p adjustments

                -- Artificially delay the mining process when not using
                -- proof-of-work mining.
                --
                unless (usePOW $ _blockChainwebVersion p) $ do
                    d <- MWC.geometric1
                         (int (order graph) / (int (_configMeanBlockTimeSeconds conf) * 1000000))
                         gen
                    threadDelay d

                -- The new block's creation time. Must come after any simulated
                -- delay.
                --
                ct <- getCurrentTimeIntegral

                -- Loops (i.e. "mines") if a non-matching nonce was generated.
                --
                -- INVARIANT: `testMine` will succeed on the first attempt when
                -- POW is not used.
                --
                testMine (Nonce nonce) target ct nid cid c >>= \case
                    Left BadNonce -> do
                        -- atomicModifyIORef' counter (\n -> (succ n, ()))
                        mine (succ nonce) adjustments'
                    Left BadAdjacents ->
                        mine nonce adjustments'
                    Right (T2 newBh newCut) -> do

                        -- DEBUGGING --
                        -- Uncomment the following for a live view of mining
                        -- results on Chain 0. You will have to uncomment a
                        -- number of surrounding helper values and readd some
                        -- imports.

                        -- total <- readIORef counter

                        -- let targetBits :: String
                        --     targetBits = printf "%0256b" $ htInteger target

                        -- when (cid == testChainId 0) $ do
                        --     printf "\n--- NODE:%02d HASHES:%06x TARGET:%s...%s HEIGHT:%03x WEIGHT:%06x PARENT:%s NEW:%s TIME:%02.2f\n"
                        --         (_nodeIdId nid)
                        --         total
                        --         (take 30 targetBits)
                        --         (drop 226 targetBits)
                        --         (pheight newBh)
                        --         (pweight newBh)
                        --         (take 8 . drop 5 . show $ _blockHash p)
                        --         (take 8 . drop 5 . show $ _blockHash newBh)
                        --         (int (time newBh - time p) / 1000000 :: Float)
                        --     hFlush stdout

                        pure $! T3 newBh newCut adjustments'

        -- Mine a new block
        --
        T3 newBh c' adjustments' <- mine nonce0 adjustments0

        logg Info $! "created new block" <> sshow i

        -- Publish the new Cut into the CutDb (add to queue).
        --
        atomically $! addCutHashes cutDb (cutToCutHashes Nothing c')

        let !limit = _blockHeight newBh - BlockHeight (int window)

        -- Since mining has been successful, we prune the
        -- `HashMap` of adjustment values that we've seen.
        --
        -- Due to this pruning, the `HashMap` should only ever
        -- contain approximately N entries, where:
        --
        -- @
        -- C := number of chains
        -- W := number of blocks in the epoch window
        --
        -- N = W * C
        -- @
        --
        go gen (i + 1) (HM.filter (\(T2 h _) -> h > limit) adjustments')

    window :: Natural
    window = _configWindowWidth conf

    getTarget
        :: ChainId
        -> BlockHeader
        -> Adjustments
        -> IO (T2 HashTarget Adjustments)
    getTarget cid bh adjustments
        | not $ usePOW (_blockChainwebVersion bh) = pure $! T2 (_blockTarget bh) adjustments
        | otherwise = case HM.lookup (_blockHash bh) adjustments of
              Just (T2 _ t) -> pure $! T2 t adjustments
              Nothing -> case blockDb cid of
                  Nothing -> pure $! T2 (_blockTarget bh) adjustments
                  Just db -> do
                      t <- hashTarget db bh
                           (BlockRate $ _configMeanBlockTimeSeconds conf)
                           (WindowWidth $ _configWindowWidth conf)
                      pure $! T2 t (HM.insert (_blockHash bh) (T2 (_blockHeight bh) t) adjustments)

    blockDb :: ChainId -> Maybe BlockHeaderDb
    blockDb cid = wcdb ^? webBlockHeaderDb . ix cid

    -- htInteger :: HashTarget -> Integer
    -- htInteger (HashTarget (PowHashNat w)) = fromIntegral w

    -- pheight :: BlockHeader -> Word64
    -- pheight bh = case _blockHeight bh of BlockHeight w -> w

    -- pweight :: BlockHeader -> Integer
    -- pweight bh = case _blockWeight bh of
    --     BlockWeight (HashDifficulty (PowHashNat w)) -> int w

    -- time :: BlockHeader -> Int64
    -- time h = case _blockCreationTime h of BlockCreationTime (Time (TimeSpan n)) -> n
