{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Miner.POW
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A true Proof of Work miner.
--

module Chainweb.Miner.POW ( powMiner ) where

import Control.Lens (ix, view, (^?), (^?!))

import qualified Data.HashMap.Strict as HM
import Data.Reflection (Given, give)
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..), T3(..))

import System.LogLevel (LogLevel(..))
import qualified System.Random.MWC as MWC

-- internal modules

import Chainweb.BlockHash (BlockHash)
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB (BlockHeaderDb)
import Chainweb.ChainId (ChainId)
import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Difficulty
import Chainweb.Miner.Config (MinerConfig(..))
import Chainweb.NodeId (NodeId)
import Chainweb.Payload.PayloadStore
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Time (getCurrentTimeIntegral)
import Chainweb.TreeDB.Difficulty (hashTarget)
import Chainweb.Utils
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService

import Data.LogMessage (LogFunction, JsonLog(..))

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
-- Miner

type Adjustments = HM.HashMap BlockHash (T2 BlockHeight HashTarget)

powMiner
    :: forall cas
    . PayloadCas cas
    => LogFunction
    -> MinerConfig
    -> NodeId
    -> CutDb cas
    -> IO ()
powMiner logFun conf nid cutDb = runForever logFun "POW Miner" $ do
    gen <- MWC.createSystemRandom
    give wcdb $ give payloadDb $ go gen 1 HM.empty
  where
    wcdb = view cutDbWebBlockHeaderDb cutDb
    payloadDb = view cutDbPayloadCas cutDb
    payloadStore = view cutDbPayloadStore cutDb

    logg :: LogLevel -> T.Text -> IO ()
    logg = logFun

    go
        :: Given WebBlockHeaderDb
        => Given (PayloadDb cas)
        => MWC.GenIO
        -> Int
        -> Adjustments
        -> IO ()
    go gen !i !adjustments0 = do

        nonce0 <- Nonce <$> MWC.uniform gen

        -- counter <- newIORef (1 :: Int)

        -- Mine a new block
        --
        T3 newBh c' adjustments' <- mine nonce0 adjustments0

        logg Info $! "created new block" <> sshow i
        logFun @(JsonLog NewMinedBlock) Info $ JsonLog (NewMinedBlock (ObjectEncoded newBh))

        -- Publish the new Cut into the CutDb (add to queue).
        --
        addCutHashes cutDb (cutToCutHashes Nothing c')

        let !wh = case window $ _blockChainwebVersion newBh of
              Just (WindowWidth w) -> BlockHeight (int w)
              Nothing -> error "POW miner used with non-POW chainweb!"
            !limit | _blockHeight newBh < wh = 0
                   | otherwise = _blockHeight newBh - wh

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

    -- | INVARIANT: A new cut, chain, and parent header is reselected after each
    -- hash failure. This ensures that the Cut the miner is working on doesn't
    -- grow stale, and cause forks. Without this condition (or a similar one),
    -- the miners cause forks quite aggressively.
    --
    mine
        :: Given WebBlockHeaderDb
        => Given (PayloadDb cas)
        => Nonce
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

        -- Loops (i.e. "mines") if a non-matching nonce was generated.
        --
        let pact = _webPactExecutionService $ _webBlockPayloadStorePact payloadStore
        payload <- _pactNewBlock pact (_configMinerInfo conf) p

        -- The new block's creation time.
        --
        let loop n = do
                ct <- getCurrentTimeIntegral
                testMineWithPayload @cas nonce target ct payload nid cid c pact >>= \case
                    Left BadNonce -> do
                        -- atomicModifyIORef' counter (\n -> (succ n, ()))
                        c' <- _cut cutDb

                        -- this comparision is still a bit expensive but fine for now. We
                        -- should let cutdb notify us. Or use a serial number or similar.
                        if c' /= c
                            then mine (succ n) adjustments'
                            else loop (succ n)

                    Left BadAdjacents -> mine nonce adjustments'

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
        loop nonce

    getTarget
        :: ChainId
        -> BlockHeader
        -> Adjustments
        -> IO (T2 HashTarget Adjustments)
    getTarget cid bh adjustments = case HM.lookup (_blockHash bh) adjustments of
        Just (T2 _ t) -> pure $! T2 t adjustments
        Nothing -> case blockDb cid of
            Nothing -> pure $! T2 (_blockTarget bh) adjustments
            Just db -> do
                t <- hashTarget db bh
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
