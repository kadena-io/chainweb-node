{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Module: Standalone.Mining
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Standalone.Mining where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Lens
import Control.Monad

import qualified Data.ByteString as BS

import Data.Bool
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.LogMessage
import Data.Reflection (Given(..), give)
import Data.Set (Set)
import Data.Tuple.Strict
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NEL
import qualified Data.Ratio as Ratio
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as Vector

import qualified Network.HTTP.Client as HTTP

import Numeric.Natural

import Servant.Client.Core

import System.LogLevel
import qualified System.Random.MWC as MWC

import Utils.Logging.Trace

-- chainweb imports

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Chainweb.MinerResources
import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Difficulty
import Chainweb.HostAddress
import Chainweb.Logger
import Chainweb.Logging.Miner
import Chainweb.Miner.Config
import Chainweb.Miner.Miners
import Chainweb.NodeId
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.Sync.WebBlockHeaderStore.Types
import Chainweb.Time
import Chainweb.TreeDB.Difficulty
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService.Types

type Adjustments = HM.HashMap BlockHash (T2 BlockHeight HashTarget)
newtype PrevBlock = PrevBlock BlockHeader

mining'
    :: forall cas. PayloadCas cas
    => (BlockHeader -> IO BlockHeader)
    -> LogFunction
    -> MinerConfig
    -> NodeId
    -> CutDb cas
    -> IO ()
mining' mine lf conf nid cdb = runForever lf "Mining Coordinator" $ do
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

        -- Mine a new Cut.
        --
        c <- _cut cdb
        T5 p newBh payload c' adj' <- raceMine c

        let bytes = foldl' (\acc (Transaction bs, _) -> acc + BS.length bs) 0 $
                    _payloadWithOutputsTransactions payload
            !nmb = NewMinedBlock
                   (ObjectEncoded newBh)
                   (int . Vector.length $ _payloadWithOutputsTransactions payload)
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
      where
        -- | Attempt to mine on the given `Cut`, until a new one should come in
        -- from the network.
        --
        raceMine :: Cut -> IO (T5 PrevBlock BlockHeader PayloadWithOutputs Cut Adjustments)
        raceMine !c = do
            ecut <- race (awaitCut cdb c) (mineCut @cas mine lf conf nid cdb g c adj)
            either raceMine pure ecut
mineCut
    :: forall cas. PayloadCas cas
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
            let bdb = fromJuste $ blockDb cid
            T2 _target adj' <- getTarget bdb (_blockChainwebVersion p) p adjustments

            -- Assemble block without Nonce and Timestamp
            --
            creationTime <- getCurrentTimeIntegral
            nonce <- Nonce <$> MWC.uniform g
            let candidateHeader = newBlockHeader
                    (nodeIdFromNodeId nid cid)
                    adjParents
                    (_payloadWithOutputsPayloadHash payload)
                    nonce
                    maxTarget
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
    wcdb :: WebBlockHeaderDb
    wcdb = view cutDbWebBlockHeaderDb cdb

    payloadStore :: WebBlockPayloadStore cas
    payloadStore = view cutDbPayloadStore cdb

    pact :: PactExecutionService
    pact = _webPactExecutionService $ _webBlockPayloadStorePact payloadStore

    blockDb :: ChainId -> Maybe BlockHeaderDb
    blockDb cid = wcdb ^? webBlockHeaderDb . ix cid

-- | Obtain a new Proof-of-Work target.
--
getTarget
    :: BlockHeaderDb
    -> ChainwebVersion
    -> BlockHeader
    -> Adjustments
    -> IO (T2 HashTarget Adjustments)
getTarget blockDb v bh as = case miningProtocol v of
    Timed -> pure testTarget
    ProofOfWork -> prodTarget
  where
    testTarget :: T2 HashTarget Adjustments
    testTarget = T2 (_blockTarget bh) mempty

    prodTarget :: IO (T2 HashTarget Adjustments)
    prodTarget = case HM.lookup (_blockHash bh) as of
        Just (T2 _ t) -> pure $ T2 t as
        Nothing -> do
            t <- hashTarget blockDb bh
            pure $ T2 t (HM.insert (_blockHash bh) (T2 (_blockHeight bh) t) as)

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

awaitCut :: CutDb cas -> Cut -> IO Cut
awaitCut cdb c = atomically $ do
    c' <- _cutStm cdb
    when (c' == c) retry
    return c'

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
estimatedHashes (PrevBlock p) b = floor $ (d Ratio.% t) * 1000000
  where
    t :: Integer
    t = case timeBetween b p of Micros t' -> int t'

    d :: Integer
    d = case targetToDifficulty $ _blockTarget b of
        HashDifficulty (PowHashNat w) -> int w


runMiner'
    :: Logger logger
    => PayloadCas cas
    => ChainwebVersion
    -> MinerResources logger cas
    -> IO ()
runMiner' v mr = do
    inner <- chooseMiner
    mining'
        inner
        (logFunction $ _minerResLogger mr)
        conf
        (_minerResNodeId mr)
        (_minerResCutDb mr)
  where
    conf :: MinerConfig
    conf = _minerResConfig mr

    miners :: MinerCount
    miners = _configTestMiners conf

    chooseMiner :: IO (BlockHeader -> IO BlockHeader)
    chooseMiner = case miningProtocol v of
        Timed -> testMiner
        ProofOfWork -> powMiner

    testMiner :: IO (BlockHeader -> IO BlockHeader)
    testMiner = do
        gen <- MWC.createSystemRandom
        pure $ localTest gen miners

    powMiner :: IO (BlockHeader -> IO BlockHeader)
    powMiner = case g $ _configRemoteMiners conf of
        Nothing -> pure $ localPOW v
        Just rs -> do
            m <- HTTP.newManager HTTP.defaultManagerSettings
            pure $ remoteMining m rs

    g :: Set HostAddress -> Maybe (NonEmpty BaseUrl)
    g = fmap (NEL.map f) . NEL.nonEmpty . S.toList

    f :: HostAddress -> BaseUrl
    f (HostAddress hn p) = BaseUrl Http hn' p' ""
      where
        hn' = T.unpack $ hostnameToText hn
        p' = fromIntegral p
