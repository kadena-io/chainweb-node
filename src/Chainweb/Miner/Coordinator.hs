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
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>, Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
--

module Chainweb.Miner.Coordinator
  ( -- * Types
    MiningState(..)
  , PrevBlock(..)
    -- * Functions
  , working
  , publishing
  ) where

import Control.Concurrent.STM (TVar, atomically, readTVarIO, retry, writeTVar)
import Control.Lens (iforM, ix, set, to, view, (^?), (^?!))
import Control.Monad (void, when)

import Data.Bool (bool)
import qualified Data.ByteString as BS
import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as HM
import Data.Ratio ((%))
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..))
import qualified Data.Vector as V

import Numeric.Natural (Natural)

import System.LogLevel (LogLevel(..))

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
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Time (Micros(..), getCurrentTimeIntegral)
import Chainweb.TreeDB.Difficulty (hashTarget)
import Chainweb.Utils hiding (check)
import Chainweb.Version
import Chainweb.WebBlockHeaderDB
import Chainweb.WebPactExecutionService

import Data.LogMessage (JsonLog(..), LogFunction)

-- -------------------------------------------------------------------------- --
-- Miner

type Adjustments = HM.HashMap BlockHash (T2 BlockHeight HashTarget)

-- | Data shared between the mining threads represented by `working` and
-- `publishing`.
--
data MiningState = MiningState
    { _msPayload :: !PayloadWithOutputs
      -- ^ The payload associated with the /current/ `BlockHeader` being mined.
    , _msBlock :: !PrevBlock
      -- ^ The parent block of the current `BlockHeader` being mined.
    }

newtype PrevBlock = PrevBlock BlockHeader

-- | THREAD: Receives new `Cut` data, and publishes it to remote mining
-- clients that obey the `MiningAPI`.
--
working
    :: forall cas. (BlockHeader -> IO ())
    -> TVar (Maybe MiningState)
    -> MinerConfig
    -> NodeId
    -> CutDb cas
    -> Adjustments
    -> IO ()
working submit tp conf nid cdb !adj = _cut cdb >>= work
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

                atomically . writeTVar tp . Just . MiningState payload $ PrevBlock p
                submit header

                -- Avoid mining on the same Cut twice.
                --
                void $ awaitNewCut cdb c
                -- TODO How often should pruning occur?
                working submit tp conf nid cdb $ filterAdjustments header adj'

filterAdjustments :: BlockHeader -> Adjustments -> Adjustments
filterAdjustments newBh as = case window $ _blockChainwebVersion newBh of
    Nothing -> mempty
    Just (WindowWidth w) ->
        let wh = BlockHeight (int w)
            limit = bool (_blockHeight newBh - wh) 0 (_blockHeight newBh < wh)
        in HM.filter (\(T2 h _) -> h > limit) as

-- | THREAD: Accepts a "solved" `BlockHeader` from some external source (likely
-- a remote mining client), reassociates it with the `Cut` from which it
-- originated, and publishes it to the `Cut` network.
--
publishing :: LogFunction -> TVar (Maybe MiningState) -> CutDb cas -> BlockHeader -> IO ()
publishing lf tp cdb bh = do
    -- Reassociate the new `BlockHeader` with the current `Cut`, if possible.
    -- Otherwise, return silently.
    --
    c <- _cut cdb
    readTVarIO tp >>= \case
        Nothing -> pure ()  -- TODO Throw error?
        Just (MiningState pl p)
            | not (samePayload pl) ->
                lf @T.Text Debug $ "Newly mined block for outdated payload"
            | otherwise -> tryMonotonicCutExtension c bh >>= \case
                Nothing -> lf @T.Text Info $ "Newly mined block for outdated cut"
                Just c' -> do
                    -- Publish the new Cut into the CutDb (add to queue).
                    --
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
    samePayload :: PayloadWithOutputs -> Bool
    samePayload pl = _blockPayloadHash bh == _payloadWithOutputsPayloadHash pl

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

getAdjacentParents :: Cut -> BlockHeader -> Maybe BlockHashRecord
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
