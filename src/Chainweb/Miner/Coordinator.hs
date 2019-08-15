{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.Miner.Coordinator
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>, Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- A true mining coordination module that can dispatch to various types of
-- mining schemes.
--

module Chainweb.Miner.Coordinator
( mining

-- * Internal
, mineCut
) where

import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Control.Monad.STM

import Data.Bool (bool)
import qualified Data.ByteString as BS
import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as HM
import Data.Ratio ((%))
import Data.Reflection (Given, give)
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..), T5(..))
import qualified Data.Vector as V

import Numeric.Natural (Natural)

import System.LogLevel (LogLevel(..))
import qualified System.Random.MWC as MWC

-- internal modules

import Chainweb.BlockHash (BlockHash, BlockHashRecord(..))
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB (BlockHeaderDb)
import Chainweb.ChainId (ChainId)
import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Difficulty
import Chainweb.Logging.Miner
import Chainweb.Miner.Config (MinerConfig(..))
import Chainweb.NodeId (NodeId, nodeIdFromNodeId)
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Time
import Chainweb.TreeDB.Difficulty (hashTarget)
import Chainweb.Utils hiding (check)
import Chainweb.Version
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService

import Data.LogMessage (JsonLog(..), LogFunction)

import Utils.Logging.Trace

-- -------------------------------------------------------------------------- --
-- Miner

type Adjustments = HM.HashMap BlockHash (T2 BlockHeight HashTarget)

newtype PrevBlock = PrevBlock BlockHeader

mining
    :: forall cas. PayloadCas cas
    => (BlockHeader -> IO BlockHeader)
    -> LogFunction
    -> MinerConfig
    -> NodeId
    -> CutDb cas
    -> IO ()
mining mine lf conf nid cdb = runForever lf "Mining Coordinator" $ do
    g <- MWC.createSystemRandom
    give wcdb $ give payloadDb $ go g 1 HM.empty
  where
    wcdb = view cutDbWebBlockHeaderDb cdb
    payloadDb = view cutDbPayloadCas cdb

    logg :: LogLevel -> T.Text -> IO ()
    logg = lf

    go
        :: Given WebBlockHeaderDb
        => Given (PayloadDb cas)
        => MWC.GenIO
        -> Int
        -> Adjustments
        -> IO ()
    go g !i !adj = do

        -- Mine a new Cut
        --
        c <- _cut cdb

        let f !x =
              race (awaitCut cdb x) (mineCut @cas mine lf conf nid cdb g x adj) >>= either f pure

        T5 p newBh payload c' adj' <- f c

        let bytes = foldl' (\acc (Transaction bs, _) -> acc + BS.length bs) 0 $
                    _payloadWithOutputsTransactions payload
            !nmb = NewMinedBlock
                   (ObjectEncoded newBh)
                   (int . V.length $ _payloadWithOutputsTransactions payload)
                   (int bytes)
                   (estimatedHashes p newBh)

        logg Info $! "Miner: created new block" <> sshow i
        lf @(JsonLog NewMinedBlock) Info $ JsonLog nmb

        -- Publish the new Cut into the CutDb (add to queue).
        --
        addCutHashes cdb $! cutToCutHashes Nothing c'
            & set cutHashesHeaders (HM.singleton (_blockHash newBh) newBh)
            & set cutHashesPayloads
                (HM.singleton (_blockPayloadHash newBh) (payloadWithOutputsToPayloadData payload))

        -- Wait for a new cut. We never mine twice on the same cut. If it stays
        -- at the same cut for a longer time, we are most likely in catchup
        -- mode.
        --
        trace lf "Miner.POW.powMiner.awaitCut" (cutIdToTextShort $ _cutId c) 1
            $ void $ awaitCut cdb c

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
        go g (i + 1) $ filterAdjustments newBh adj'

filterAdjustments :: BlockHeader -> Adjustments -> Adjustments
filterAdjustments newBh as = case window $ _blockChainwebVersion newBh of
    Nothing -> mempty
    Just (WindowWidth w) ->
        let wh = BlockHeight (int w)
            limit = bool (_blockHeight newBh - wh) 0 (_blockHeight newBh < wh)
        in HM.filter (\(T2 h _) -> h > limit) as

-- | The estimated per-second Hash Power of the network, guessed from the time
-- it took to mine this block among all miners on the chain.
--
estimatedHashes :: PrevBlock -> BlockHeader -> Natural
estimatedHashes (PrevBlock p) b = floor $ (d % t) * 1000000
  where
    t :: Integer
    t = case timeBetween b p of Micros t' -> int t'

    d :: Integer
    d = case targetToDifficulty $ _blockTarget b of
        HashDifficulty (PowHashNat w) -> int w

awaitCut :: CutDb cas -> Cut -> IO Cut
awaitCut cdb c = atomically $ do
    c' <- _cutStm cdb
    when (c' == c) retry
    return c'

mineCut
    :: PayloadCas cas
    => Given WebBlockHeaderDb
    => Given (PayloadDb cas)
    => (BlockHeader -> IO BlockHeader)
    -> LogFunction
    -> MinerConfig
    -> NodeId
    -> CutDb cas
    -> MWC.GenIO
    -> Cut
    -> Adjustments
    -> IO (T5 PrevBlock BlockHeader PayloadWithOutputs Cut Adjustments)
mineCut mine logfun conf nid cdb g !c !adjustments = do

    -- Randomly pick a chain to mine on.
    --
    cid <- randomChainId c

    -- The parent block the mine on. Any given chain will always
    -- contain at least a genesis block, so this otherwise naughty
    -- `^?!` will always succeed.
    --
    let !p = c ^?! ixg cid

    -- check if chain can be mined on (check adjacent parents)
    --
    case getAdjacentParents c p of

        Nothing -> mineCut mine logfun conf nid cdb g c adjustments
            -- spin until a chain is found that isn't blocked

        Just adjParents -> do

            -- get payload
            --
            payload <- trace logfun "Miner.POW.mineCut._pactNewBlock" (_blockHash p) 1
                $ _pactNewBlock pact (_configMinerInfo conf) p

            -- get target
            --
            T2 target adj' <- getTarget (_blockChainwebVersion p) cid p adjustments

            -- Assemble block without Nonce and Timestamp
            --
            creationTime <- getCurrentTimeIntegral
            nonce <- Nonce <$> MWC.uniform g
            let candidateHeader = newBlockHeader
                    (nodeIdFromNodeId nid cid)
                    adjParents
                    (_payloadWithOutputsPayloadHash payload)
                    nonce
                    target
                    creationTime
                    p

            newHeader <- mine candidateHeader

            -- create cut with new block
            --
            -- This is expected to succeed, since the cut invariants should
            -- hold by construction
            --
            !c' <- monotonicCutExtension c newHeader

            return $! T5 (PrevBlock p) newHeader payload c' adj'
  where
    wcdb = view cutDbWebBlockHeaderDb cdb
    payloadStore = view cutDbPayloadStore cdb

    pact :: PactExecutionService
    pact = _webPactExecutionService $ _webBlockPayloadStorePact payloadStore

    blockDb :: ChainId -> Maybe BlockHeaderDb
    blockDb cid = wcdb ^? webBlockHeaderDb . ix cid

    -- TODO What's with the discrepancy between `as` and `adjustments` here?
    -- Which is the true container we wish to manipulate?
    getTarget
        :: ChainwebVersion
        -> ChainId
        -> BlockHeader
        -> Adjustments
        -> IO (T2 HashTarget Adjustments)
    getTarget v cid bh as = case miningProtocol v of
        Timed -> testTarget
        ProofOfWork -> prodTarget
      where
        testTarget = pure $ T2 (_blockTarget bh) mempty
        prodTarget = case HM.lookup (_blockHash bh) as of
          Just (T2 _ t) -> pure $! T2 t adjustments
          Nothing -> case blockDb cid of
            Nothing -> pure $! T2 (_blockTarget bh) adjustments
            Just db -> do
              t <- hashTarget db bh
              pure $! T2 t (HM.insert (_blockHash bh) (T2 (_blockHeight bh) t) adjustments)


-- -------------------------------------------------------------------------- --
--

getAdjacentParents
    :: (IxedGet s, IxValue s ~ BlockHeader, Index s ~ ChainId)
    => s
    -> BlockHeader
    -> Maybe BlockHashRecord
getAdjacentParents c p = BlockHashRecord <$> newAdjHashes
  where
    -- | Try to get all adjacent hashes dependencies.
    --
    newAdjHashes :: Maybe (HM.HashMap ChainId BlockHash)
    newAdjHashes = iforM (_getBlockHashRecord $ _blockAdjacentHashes p) $ \xcid _ ->
        c ^?! ixg xcid . to (tryAdj (_blockHeight p))

    tryAdj :: BlockHeight -> BlockHeader -> Maybe BlockHash
    tryAdj h b
        | _blockHeight b == h = Just $! _blockHash b
        | _blockHeight b == h + 1 = Just $! _blockParent b
        | otherwise = Nothing
