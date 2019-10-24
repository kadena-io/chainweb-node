{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , MiningStats(..)
  , PrevTime(..)
  , ChainChoice(..)
    -- * Functions
  , newWork
  , publish
  ) where

import Data.Aeson (ToJSON)
import Data.Bool (bool)

import Control.DeepSeq (NFData)
import Control.Error.Util ((!?), (??))
import Control.Lens (iforM, set, to, (^.), (^?!))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)

import qualified Data.ByteString as BS
import Data.Foldable (foldl')
import Data.Generics.Wrapped (_Unwrapped)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Ratio ((%))
import qualified Data.Text as T
import Data.Tuple.Strict (T3(..))
import qualified Data.Vector as V

import GHC.Generics (Generic)

import Numeric.Natural (Natural)

import System.LogLevel (LogLevel(..))

-- internal modules

import Chainweb.BlockHash (BlockHash, BlockHashRecord(..))
import Chainweb.BlockHeader
import Chainweb.Cut
import Chainweb.Cut.CutHashes
import Chainweb.CutDB
import Chainweb.Difficulty
import Chainweb.Logging.Miner
import Chainweb.Miner.Pact (Miner, minerId)
import Chainweb.Payload
import Chainweb.Sync.WebBlockHeaderStore
import Chainweb.Time (Micros(..), getCurrentTimeIntegral)
import Chainweb.Utils hiding (check)
import Chainweb.Version

import Data.LogMessage (JsonLog(..), LogFunction)

-- -------------------------------------------------------------------------- --
-- Miner

-- | Data shared between the mining threads represented by `newWork` and
-- `publish`.
--
newtype MiningState =
    MiningState (M.Map BlockPayloadHash (T3 Miner PrevTime PayloadWithOutputs))
    deriving stock (Generic)
    deriving newtype (Semigroup, Monoid)

-- | For logging during `MiningState` manipulation.
--
data MiningStats = MiningStats
    { _statsCacheSize :: Int
    , _states503s :: Int
    , _statsAvgTxs :: Int }
    deriving stock (Generic)
    deriving anyclass (ToJSON, NFData)

-- | The `BlockCreationTime` of the parent of some current, "working"
-- `BlockHeader`.
--
newtype PrevTime = PrevTime BlockCreationTime

data ChainChoice = Anything | TriedLast ChainId | Suggestion ChainId

-- | Construct a new `BlockHeader` to mine on.
--
newWork
    :: ChainChoice
    -> Miner
    -> PactExecutionService
    -> Cut
    -> IO (T3 PrevTime BlockHeader PayloadWithOutputs)
newWork choice miner pact c = do
    -- Randomly pick a chain to mine on, unless the caller specified a specific
    -- one.
    --
    cid <- chainChoice c choice

    -- The parent block the mine on. Any given chain will always
    -- contain at least a genesis block, so this otherwise naughty
    -- `^?!` will always succeed.
    --
    let !p = c ^?! ixg cid

    -- Check if the chain can be mined on by determining if adjacent parents
    -- also exist. If they don't, we test other chains on this same `Cut`,
    -- since we still believe this `Cut` to be good.
    --
    -- Note that if the caller did specify a specific chain to mine on, we only
    -- attempt this once. We assume they'd rather have /some/ work on /some/
    -- chain rather than no work on their chosen chain. This will also help
    -- rebalance "selfish" mining, for remote clients who claim that they want
    -- to focus their hash power on a certain chain.
    --
    -- TODO Consider instead some maximum amount of retries?
    --
    case getAdjacentParents c p of
        Nothing -> newWork (TriedLast cid) miner pact c
        Just adjParents -> do
            -- Fetch a Pact Transaction payload. This is an expensive call
            -- that shouldn't be repeated.
            --
            creationTime <- getCurrentTimeIntegral
            payload <- _pactNewBlock pact miner p (BlockCreationTime creationTime)

            -- Assemble a candidate `BlockHeader` without a specific `Nonce`
            -- value. `Nonce` manipulation is assumed to occur within the
            -- core Mining logic.
            --
            let !phash = _payloadWithOutputsPayloadHash payload
                !header = newBlockHeader adjParents phash (Nonce 0) creationTime p

            pure $ T3 (PrevTime $ _blockCreationTime p) header payload

chainChoice :: Cut -> ChainChoice -> IO ChainId
chainChoice c choice = case choice of
    Anything -> randomChainId c
    Suggestion cid -> pure cid
    TriedLast cid -> loop cid
  where
    loop :: ChainId -> IO ChainId
    loop cid = do
      new <- randomChainId c
      bool (pure new) (loop cid) $ new == cid

-- | KILLSWITCH: This extra logic involving `txSilenceDates` is to be removed in
-- a future version of Chainweb. It prevents this Node from generating any new
-- Cuts after a specified date.
--
publish :: LogFunction -> MiningState -> CutDb cas -> BlockHeader -> IO ()
publish lf ms cdb bh = do
    now <- getCurrentTimeIntegral
    case txSilenceDates $ _chainwebVersion bh of
        Just end | now > end -> pure ()
        _ -> publish' lf ms cdb bh

-- | Accepts a "solved" `BlockHeader` from some external source (e.g. a remote
-- mining client), attempts to reassociate it with the current best `Cut`, and
-- publishes the result to the `Cut` network.
--
publish' :: LogFunction -> MiningState -> CutDb cas -> BlockHeader -> IO ()
publish' lf (MiningState ms) cdb bh = do
    c <- _cut cdb
    let !phash = _blockPayloadHash bh
    res <- runExceptT $ do
        T3 m p pl <- M.lookup phash ms ?? "BlockHeader given with no associated Payload"
        let !miner = m ^. minerId . _Unwrapped
        c' <- tryMonotonicCutExtension c bh !? ("Newly mined block for outdated cut: " <> miner)
        lift $ do
            -- Publish the new Cut into the CutDb (add to queue).
            --
            addCutHashes cdb $ cutToCutHashes Nothing c'
                & set cutHashesHeaders (HM.singleton (_blockHash bh) bh)
                & set cutHashesPayloads (HM.singleton phash (payloadWithOutputsToPayloadData pl))

            -- Log mining success.
            --
            let bytes = foldl' (\acc (Transaction bs, _) -> acc + BS.length bs) 0 $
                        _payloadWithOutputsTransactions pl

            pure . JsonLog $ NewMinedBlock
                (ObjectEncoded bh)
                (int . V.length $ _payloadWithOutputsTransactions pl)
                (int bytes)
                (estimatedHashes p bh)
                miner
    either (lf @T.Text Info) (lf Info) res

-- | The estimated per-second Hash Power of the network, guessed from the time
-- it took to mine this block among all miners on the chain.
--
estimatedHashes :: PrevTime -> BlockHeader -> Natural
estimatedHashes (PrevTime p) b = floor $ (d % t) * 1000000
  where
    t :: Integer
    t = case timeBetween (_blockCreationTime b) p of Micros t' -> int t'

    d :: Integer
    d = case targetToDifficulty $ _blockTarget b of
        HashDifficulty (PowHashNat w) -> int w

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
