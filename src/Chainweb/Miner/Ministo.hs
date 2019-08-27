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
-- Module: Chainweb.Miner.Ministo
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>, Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
--

module Chainweb.Miner.Ministo
  ( mippies
  , coordination
  , working
  , publishing

  -- * Internal
  , mineCut
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM (TVar, readTVarIO, writeTVar)
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
import Chainweb.Miner.Core (HeaderBytes(..))
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

type Prev = T2 PayloadWithOutputs PrevBlock

newtype PrevBlock = PrevBlock BlockHeader

-- | THREAD: Receives new `Cut` data, and publishes it to remote mining
-- clients that obey the `MiningAPI`.
--
working
    :: forall cas. (BlockHeader -> IO ())
    -> TVar (Maybe Prev)
    -> NodeId
    -> MinerConfig
    -> CutDb cas
    -> Adjustments
    -> IO ()
working submit tp nid conf cdb adj = _cut cdb >>= work
  where
    pact :: PactExecutionService
    pact = _webPactExecutionService . _webBlockPayloadStorePact $ view cutDbPayloadStore cdb

    blockDb :: ChainId -> Maybe BlockHeaderDb
    blockDb cid = cdb ^? cutDbWebBlockHeaderDb . webBlockHeaderDb . ix cid

    work :: Cut -> IO ()
    work c = do
        -- Randomly pick a chain to mine on.
        --
        cid <- randomChainId c

        -- The parent block the mine on. Any given chain will always
        -- contain at least a genesis block, so this otherwise naughty
        -- `^?!` will always succeed.
        --
        let !p = c ^?! ixg cid

        -- Check if the chain can be mined on by determining if adjacent parents
        -- also exist. If they don't, we test other chains on this same `Cut`,
        -- since we still believe this `Cut` to be good.
        --
        case getAdjacentParents c p of
            Nothing -> work c
            Just adjParents -> do
                -- Fetch a Pact Transaction payload. This is an expensive call
                -- that shouldn't be repeated.
                --
                payload <- _pactNewBlock pact (_configMinerInfo conf) p

                -- Potentially perform Difficulty Adjustment to determine the
                -- `HashTarget` we are to use in mining.
                --
                let bdb = fromJuste $ blockDb cid
                T2 target adj' <- getTarget bdb (_blockChainwebVersion p) p adj

                -- Assemble a candidate `BlockHeader` without a specific `Nonce`
                -- value. `Nonce` manipulation is assumed to occur within the
                -- core Mining logic.
                --
                creationTime <- getCurrentTimeIntegral
                let !phash = _payloadWithOutputsPayloadHash payload
                    !header = newBlockHeader
                        (nodeIdFromNodeId nid cid)
                        adjParents
                        phash
                        (Nonce 0)  -- TODO Confirm that this is okay.
                        target
                        creationTime
                        p

                submit header
                atomically . writeTVar tp . Just . T2 payload $ PrevBlock p

                -- Avoid mining on the same Cut twice.
                --
                void $ awaitNewCut cdb c
                working submit tp nid conf cdb adj'

-- | THREAD: Accepts "solved" `BlockHeader` bytes from some external source
-- (likely a remote mining client), reassociates it with the `Cut` from
-- which it originated, and publishes it to the `Cut` network.
--
publishing :: LogFunction -> TVar (Maybe Prev) -> CutDb cas -> HeaderBytes -> IO ()
publishing lf tp cdb (HeaderBytes hbytes) = do
    -- TODO Catch decoding error and send failure code?
    bh <- runGet decodeBlockHeaderWithoutHash hbytes

    -- Reassociate the new `BlockHeader` with the current `Cut`, if possible.
    -- Otherwise, return silently.
    --
    c <- _cut cdb
    readTVarIO tp >>= \case
        Nothing -> pure ()  -- TODO Throw error?
        Just (T2 pl p) -> when (compatibleCut c bh && samePayload bh pl) $ do
            -- Publish the new Cut into the CutDb (add to queue).
            --
            c' <- monotonicCutExtension c bh
            addCutHashes cdb $ cutToCutHashes Nothing c'
                & set cutHashesHeaders
                    (HM.singleton (_blockHash bh) bh)
                & set cutHashesPayloads
                    (HM.singleton (_blockPayloadHash bh) (payloadWithOutputsToPayloadData pl))

            -- Log mining success.
            --
            let bytes = foldl' (\acc (Transaction bs, _) -> acc + BS.length bs) 0 $
                    _payloadWithOutputsTransactions pl
                !nmb = NewMinedBlock
                       (ObjectEncoded bh)
                       (int . V.length $ _payloadWithOutputsTransactions pl)
                       (int bytes)
                       (estimatedHashes p bh)

            lf @(JsonLog NewMinedBlock) Info $ JsonLog nmb
  where
    -- | Even if a `Cut` is not the original `Cut` from which a `BlockHeader`
    -- originated, they might still be compatible. If the highest header in the
    -- Cut matches the parent of new header (for the chain that they share),
    -- then this Cut is still compatible, and we haven't wasted mining effort.
    --
    compatibleCut :: Cut -> BlockHeader -> Bool
    compatibleCut c bh = case HM.lookup (_blockChainId bh) $ _cutMap c of
        Nothing -> False
        -- TODO Is height a sufficient check, or should it go by parent hash?
        Just cb -> int (_blockHeight bh) - int (_blockHeight cb) == (1 :: Integer)

    samePayload :: BlockHeader -> PayloadWithOutputs -> Bool
    samePayload bh pl = _blockPayloadHash bh == _payloadWithOutputsPayloadHash pl

-- | Coordinate the submission and collection of all mining work.
--
coordination :: forall cas. PayloadCas cas => CutDb cas -> IO ()
coordination = undefined

mippies
    :: forall cas. PayloadCas cas
    => (BlockHeader -> IO BlockHeader)
    -> LogFunction
    -> MinerConfig
    -> NodeId
    -> CutDb cas
    -> IO ()
mippies mine lf conf nid cdb = runForever lf "Mining Coordinator" $ do
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
        trace lf "Miner.POW.powMiner.awaitNewCut" (cutIdToTextShort $ _cutId c) 1
            $ void $ awaitNewCut cdb c

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
            ecut <- race (awaitNewCut cdb c) (mineCut @cas mine lf conf nid cdb g c adj)
            either raceMine pure ecut

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

awaitNewCut :: CutDb cas -> Cut -> IO Cut
awaitNewCut cdb c = atomically $ do
    c' <- _cutStm cdb
    when (c' == c) retry
    return c'

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
            T2 target adj' <- getTarget bdb (_blockChainwebVersion p) p adjustments

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
