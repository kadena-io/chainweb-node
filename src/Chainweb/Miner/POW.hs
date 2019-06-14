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
-- Module: Chainweb.Miner.POW
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- A true Proof of Work miner.
--

module Chainweb.Miner.POW
( powMiner

-- * Internal
, mineCut
, mine
) where

import Control.Concurrent.Async
import Control.Lens
import Control.Monad
import Control.Monad.STM

import Crypto.Hash.Algorithms
import Crypto.Hash.IO

import qualified Data.ByteArray as BA
import Data.Bytes.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString as B
import Data.Foldable (foldl')
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Proxy
import Data.Reflection (Given, give)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Tuple.Strict (T2(..))
import Data.Word

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

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

-- -------------------------------------------------------------------------- --
-- Miner

-- | The result of mining a new `Cut`.
--
data Ore = Ore !BlockHeader !PayloadWithOutputs !Cut !Adjustments !Word

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

        -- Mine a new Cut
        --
        c <- _cut cutDb
        Ore newBh payload c' adjustments' hashAttempts <- do
            let go2 !x = race (awaitNextCut cutDb x) (mineCut @cas logFun conf nid cutDb gen x adjustments0) >>= \case
                    Left c' -> go2 c'
                    Right !r -> return r
            go2 c

        let bytes = foldl' (\acc (Transaction bs, _) -> acc + BS.length bs) 0 $
                    _payloadWithOutputsTransactions payload
            !nmb = NewMinedBlock
                   (ObjectEncoded newBh)
                   (int . Seq.length $ _payloadWithOutputsTransactions payload)
                   (int bytes)
                   hashAttempts

        logg Info $! "POW Miner: created new block" <> sshow i
        logFun @(JsonLog NewMinedBlock) Info $ JsonLog nmb

        -- Publish the new Cut into the CutDb (add to queue).
        --
        addCutHashes cutDb (cutToCutHashes Nothing c')

        -- Wait for a new cut. We never mine twice on the same cut. If it stays
        -- at the same cut for a longer time, we are most likely in catchup
        -- mode.
        --
        void $ awaitNextCut cutDb c

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

awaitNextCut :: CutDb cas -> Cut -> IO Cut
awaitNextCut cutDb c = atomically $ do
    c' <- _cutStm cutDb
    when (c' == c) retry
    return c'

mineCut
    :: PayloadCas cas
    => Given WebBlockHeaderDb
    => Given (PayloadDb cas)
    => LogFunction
    -> MinerConfig
    -> NodeId
    -> CutDb cas
    -> MWC.GenIO
    -> Cut
    -> Adjustments
    -> IO Ore
mineCut logfun conf nid cutDb gen !c !adjustments = do

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

        Nothing -> mineCut logfun conf nid cutDb gen c adjustments
            -- spin until a chain is found that isn't blocked

        Just adjParents -> do

            -- get payload
            payload <- _pactNewBlock pact (_configMinerInfo conf) p

            -- get target
            --
            T2 target adjustments' <- getTarget cid p adjustments

            -- Assemble block without Nonce and Timestamp
            --
            creationTime <- getCurrentTimeIntegral
            nonce <- Nonce <$> MWC.uniform gen
            let candidateHeader = newBlockHeader
                    (nodeIdFromNodeId nid cid)
                    adjParents
                    (_payloadWithOutputsPayloadHash payload)
                    nonce
                    target
                    creationTime
                    p

            T2 newHeader hashAttempts <- usePowHash v mine candidateHeader nonce

            -- create cut with new block
            --
            -- This is expected to succeed, since the cut invariants should
            -- hold by construction
            --
            !c' <- monotonicCutExtension c newHeader

            -- Validate payload
            --
            logg Info $! "validate block payload"
            validatePayload newHeader payload
            logg Info $! "add block payload to payload cas"
            addNewPayload payloadDb payload

            logg Info $! "add block to payload db"
            insertWebBlockHeaderDb newHeader

            return $! Ore newHeader payload c' adjustments' hashAttempts
  where
    v = _chainwebVersion cutDb
    wcdb = view cutDbWebBlockHeaderDb cutDb
    payloadDb = view cutDbPayloadCas cutDb
    payloadStore = view cutDbPayloadStore cutDb

    pact :: PactExecutionService
    pact = _webPactExecutionService $ _webBlockPayloadStorePact payloadStore

    logg :: LogLevel -> T.Text -> IO ()
    logg = logfun

    blockDb :: ChainId -> Maybe BlockHeaderDb
    blockDb cid = wcdb ^? webBlockHeaderDb . ix cid

    validatePayload :: BlockHeader -> PayloadWithOutputs -> IO ()
    validatePayload h o = void . _pactValidateBlock pact h $ toPayloadData o

    getTarget
        :: ChainId
        -> BlockHeader
        -> Adjustments
        -> IO (T2 HashTarget Adjustments)
    getTarget cid bh as = case HM.lookup (_blockHash bh) as of
        Just (T2 _ t) -> pure $! T2 t adjustments
        Nothing -> case blockDb cid of
            Nothing -> pure $! T2 (_blockTarget bh) adjustments
            Just db -> do
                t <- hashTarget db bh
                pure $! T2 t (HM.insert (_blockHash bh) (T2 (_blockHeight bh) t) adjustments)

    toPayloadData :: PayloadWithOutputs -> PayloadData
    toPayloadData d = PayloadData
              { _payloadDataTransactions = fst <$> _payloadWithOutputsTransactions d
              , _payloadDataMiner = _payloadWithOutputsMiner d
              , _payloadDataPayloadHash = _payloadWithOutputsPayloadHash d
              , _payloadDataTransactionsHash = _payloadWithOutputsTransactionsHash d
              , _payloadDataOutputsHash = _payloadWithOutputsOutputsHash d
              }

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

-- -------------------------------------------------------------------------- --
-- Inner Mining loop

usePowHash :: ChainwebVersion -> (forall a . HashAlgorithm a => Proxy a -> f) -> f
usePowHash Test{} f = f $ Proxy @SHA512t_256
usePowHash TimedConsensus{} f = f $ Proxy @SHA512t_256
usePowHash PowConsensus{} f = f $ Proxy @SHA512t_256
usePowHash TimedCPM{} f = f $ Proxy @SHA512t_256
usePowHash Testnet00{} f = f $ Proxy @SHA512t_256
usePowHash Testnet01{} f = f $ Proxy @SHA512t_256

-- | This Miner makes low-level assumptions about the chainweb protocol. It may
-- break if the protocol changes.
--
-- TODO: Check the chainweb version to make sure this function can handle the
-- respective version.
--
mine
    :: forall a
    . HashAlgorithm a
    => Proxy a
    -> BlockHeader
    -> Nonce
    -> IO (T2 BlockHeader Word)
mine _ h nonce = do
    counter <- newIORef 0
    res <- BA.withByteArray initialTargetBytes $ \trgPtr -> do
        !ctx <- hashMutableInit @a
        bytes <- BA.copy initialBytes $ \buf ->
            allocaBytes (powSize :: Int) $ \pow -> do

                -- inner mining loop
                --
                -- We do 100000 hashes before we update the creation time.
                --
                let go 100000 !n = do

                        -- update the block creation time
                        ct <- getCurrentTimeIntegral
                        injectTime ct buf
                        go 0 n

                    go !i !n = do

                        -- increment hash counter
                        modifyIORef' counter (+ 1)

                        -- Compute POW hash for the nonce
                        injectNonce n buf
                        hash ctx buf pow

                        -- check whether the nonce meets the target
                        fastCheckTarget trgPtr (castPtr pow) >>= \case
                            True -> return ()
                            False -> go (succ i) (succ n)

                -- Start inner mining loop
                go (0 :: Int) nonce

        -- On success: deserialize and return the new BlockHeader
        runGet decodeBlockHeaderWithoutHash bytes
    T2 res <$> readIORef counter
  where
    !initialBytes = runPutS $ encodeBlockHeaderWithoutHash h
    !bufSize = B.length initialBytes
    !target = _blockTarget h
    !initialTargetBytes = runPutS $ encodeHashTarget target
    !powSize = int $ hashDigestSize @a undefined

    --  Compute POW hash
    hash :: MutableContext a -> Ptr Word8 -> Ptr Word8 -> IO ()
    hash ctx buf pow = do
        hashMutableReset ctx
        BA.withByteArray ctx $ \ctxPtr -> do
            hashInternalUpdate @a ctxPtr buf (int bufSize)
            hashInternalFinalize ctxPtr (castPtr pow)
    {-# INLINE hash #-}

    injectTime :: Time Int64 -> Ptr Word8 -> IO ()
    injectTime t buf = pokeByteOff buf 8 $ encodeTimeToWord64 t
    {-# INLINE injectTime #-}

    injectNonce :: Nonce -> Ptr Word8 -> IO ()
    injectNonce n buf = poke (castPtr buf) $ encodeNonceToWord64 n
    {-# INLINE injectNonce #-}

    -- | `PowHashNat` interprets POW hashes as unsigned 256 bit integral numbers
    -- in little endian encoding.
    --
    fastCheckTarget :: Ptr Word64 -> Ptr Word64 -> IO Bool
    fastCheckTarget !trgPtr !powPtr =
        fastCheckTargetN 3 trgPtr powPtr >>= \case
            LT -> return False
            GT -> return True
            EQ -> fastCheckTargetN 2 trgPtr powPtr >>= \case
                LT -> return False
                GT -> return True
                EQ -> fastCheckTargetN 1 trgPtr powPtr >>= \case
                    LT -> return False
                    GT -> return True
                    EQ -> fastCheckTargetN 0 trgPtr powPtr >>= \case
                        LT -> return False
                        GT -> return True
                        EQ -> return True
    {-# INLINE fastCheckTarget #-}

    fastCheckTargetN :: Int -> Ptr Word64 -> Ptr Word64 -> IO Ordering
    fastCheckTargetN n trgPtr powPtr = compare
        <$> peekElemOff trgPtr n
        <*> peekElemOff powPtr n
    {-# INLINE fastCheckTargetN #-}
