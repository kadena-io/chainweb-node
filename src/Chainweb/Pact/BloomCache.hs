{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This structure records a probabilistic sketch of which transactions were
-- processed in recent blocks. We keep block bloom filter sketches in rocksdb
-- as well as a (fixed) maximum block horizon in memory, i.e. if
--
--   h = min_height_in_cut,
--
-- the in-memory cache will not contain sketches for blocks smaller than h - d.
--
-- The cache is updated by a worker thread that listens for changes to the Cut
-- TVar.

module Chainweb.Pact.BloomCache
( TransactionBloomCache
, createCache
, destroyCache
, withCache
, member
) where

------------------------------------------------------------------------------
import Control.Applicative
import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (SomeAsyncException(..), evaluate)
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.Array.IArray as U
import Data.BloomFilter (Bloom)
import qualified Data.BloomFilter as Bloom
import qualified Data.BloomFilter.Easy as Bloom
import qualified Data.BloomFilter.Hash as Bloom
import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.ByteString.Char8 as B
import Data.Foldable
import Data.Function
import Data.Hashable
import Data.Hashable (hashWithSalt)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Int
import Data.List (lookup)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics
import GHC.Stack (HasCallStack)
import Pact.Types.Command
import qualified Pact.Types.Hash as H
------------------------------------------------------------------------------
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.CutDB (CutDb)
import qualified Chainweb.CutDB as CutDB
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import qualified Chainweb.TreeDB as TreeDB
import Chainweb.Utils (fromJuste, runGet)
import Data.CAS
import Data.CAS.RocksDB
import qualified Data.CAS.RocksDB as RDB
------------------------------------------------------------------------------

instance Bloom.Hashable H.Hash where
    hashIO32 (H.Hash bytes) salt =
        return $! fromIntegral $ hashWithSalt (fromIntegral salt) (hashCode :: Int)
      where
        hashCode = either error id $ runGetS (fromIntegral <$> getWord64host) (B.take 8 bytes)
    {-# INLINE hashIO32 #-}

    hashIO64 (H.Hash bytes) salt =
        return $! fromIntegral $ hashWithSalt (fromIntegral salt) hashCode
      where
        hashCode = either error id $ runGetS getWord64host (B.take 8 bytes)
    {-# INLINE hashIO64 #-}


data BloomEntry = BloomEntry
    { _beKey :: {-# UNPACK #-} !BloomKey
    , _beNumHashes :: {-# UNPACK #-} !Int16
    , _beBloom :: !(Bloom H.Hash)
    }
    deriving (Show, Generic)

instance Eq BloomEntry where
    (==) = (==) `on` _beKey
    {-# INLINE (==) #-}

instance Hashable BloomEntry where
    hashWithSalt s = hashWithSalt s . _beKey
    {-# INLINE hashWithSalt #-}

instance Ord BloomEntry where
    compare = compare `on` _beKey
    {-# INLINE compare #-}

instance IsCasValue BloomEntry where
    type CasKeyType BloomEntry = BloomKey
    casKey = _beKey
    {-# INLINE casKey #-}


type BloomCas = RocksDbCas BloomEntry


data BloomKey = BloomKey {
    _kHeight :: {-# UNPACK #-} !BlockHeight
  , _kHash :: {-# UNPACK #-} !BlockHash
  }
  deriving (Eq, Ord, Generic, Show)

instance Hashable BloomKey where
    hashWithSalt s (BloomKey a b) = hashWithSalt (hashWithSalt s a) b
    {-# INLINE hashWithSalt #-}


type BloomMap = HashMap BloomKey BloomEntry


data TransactionBloomCache = TransactionBloomCache {
    _map :: !(MVar BloomMap)
  , _cas :: !BloomCas
  , _thread :: !(Async ())
  }


maxBloomAssocs :: Int
maxBloomAssocs = 32678


putBloomKey :: MonadPut m => BloomKey -> m ()
putBloomKey (BloomKey h hsh) = encodeBlockHeightBe h >> encodeBlockHash hsh
{-# INLINE putBloomKey #-}


getBloomKey :: MonadGet m => m BloomKey
getBloomKey = BloomKey <$!> decodeBlockHeightBe <*> decodeBlockHash
{-# INLINE getBloomKey #-}


encodeBloomEntry :: BloomEntry -> B.ByteString
encodeBloomEntry b = runPutS $ do
    putBloomKey $ _beKey b
    putWord16le $ fromIntegral $! _beNumHashes b
    -- can't do anything better here, maybe we have to do this check elsewhere
    when (ual > maxBloomAssocs) $ fail "bloom too big"
    putWord16le $! fromIntegral bloomBits
    putWord32le $! fromIntegral lo
    putWord32le $! fromIntegral hi
    putWord32le $! fromIntegral $! length assocs
    traverse_ encOne assocs
  where
    encOne (!i, !h) = do
        putWord64le $! fromIntegral i
        putWord32le h
    bloom = _beBloom b
    ua = Bloom.bitArray bloom
    assocs = U.assocs ua
    ual = length assocs
    (lo, hi) = U.bounds ua
    bloomBits = Bloom.length bloom
{-# INLINE encodeBloomEntry #-}


decodeBloomEntry :: MonadThrow m => B.ByteString -> m BloomEntry
decodeBloomEntry = runGet $ do
    !k <- getBloomKey
    !nh <- fromIntegral <$> getWord16le
    !bloomBits <- fromIntegral <$> getWord16le
    !lo <- fromIntegral <$> getWord32le
    !hi <- fromIntegral <$> getWord32le
    !na <- fromIntegral <$> getWord32le
    guard $ na <= maxBloomAssocs
    assocs <- replicateM na decOne
    let !bits = U.array (lo, hi) assocs
    let !bloom = (Bloom.empty (Bloom.cheapHashes nh) bloomBits) { Bloom.bitArray = bits }
    return $! BloomEntry k (fromIntegral nh) bloom

  where
    decOne = do
        !i <- fromIntegral <$> getWord64le
        !h <- getWord32le
        return $! (i, h)
{-# INLINE decodeBloomEntry #-}


bloomEntryCodec :: RDB.Codec BloomEntry
bloomEntryCodec = RDB.Codec encodeBloomEntry decodeBloomEntry


bloomKeyCodec :: RDB.Codec BloomKey
bloomKeyCodec = RDB.Codec encodeBloomKey decodeBloomKey
  where
    encodeBloomKey = runPutS . putBloomKey
    decodeBloomKey :: MonadThrow m => B.ByteString -> m BloomKey
    decodeBloomKey = runGet getBloomKey


createCache
    :: PayloadCas cas
    => CutDb cas
    -> RocksDb
    -> [(ChainId, BlockHeaderDb)]
    -> IO TransactionBloomCache
createCache cutDb rocks bdbs = mask_ $ do
    !m <- newMVar HashMap.empty
    !t <- Async.asyncWithUnmask $ threadProc m
    return $! TransactionBloomCache m cas t

  where
    cas = newCas rocks bloomEntryCodec bloomKeyCodec ["BlockTxBloom"]

    threadProc mapVar restore = begin
      where
        begin = silently . restore $ go Nothing
        go !lastCut = do
            !cut <- waitForNewCut cutDb lastCut
            update cutDb cas bdbs mapVar cut (reap $! boundHeight $ lowestHeight cut)
            go $! Just cut
        silently = flip catches [
                -- cancellation msg should quit, all others restart
                Handler (\(_::SomeAsyncException) -> return ())
              , Handler (\(_::SomeException) -> begin) -- TODO: log here and delay
              ]

    lowestHeight = foldl' f (BlockHeight maxBound) . HashMap.toList . _cutMap
      where
        f bh (_, hdr) = min bh $ _blockHeight hdr

    reap !minHeight = HashMap.filterWithKey (const . (>= minHeight) . _kHeight)


destroyCache :: TransactionBloomCache -> IO ()
destroyCache = Async.cancel . _thread


withCache
    :: PayloadCas cas
    => CutDb cas
    -> RocksDb
    -> [(ChainId, BlockHeaderDb)] -- TODO: should be WebBlockHeaderDb or WebBlockHeaderStore
    -> (TransactionBloomCache -> IO a)
    -> IO a
withCache cutDb rocks bdbs = bracket (createCache cutDb rocks bdbs) destroyCache


-- | (Probabilistic) bloom membership. Looks up the given pact tx id in the
-- given consensus block. Returns `False` if the given tx definitely doesn't
-- belong in the given block (and so we don't have to decode and search it), or
-- `True` if we think there's a chance the tx might be in the block or if the
-- block is not in bloom cache.
member :: H.Hash -> (BlockHeight, BlockHash) -> TransactionBloomCache -> IO Bool
member h (a,b) (TransactionBloomCache mv cas _) = do
    !mp <- readMVar mv
    -- N.B. return false positive on block missing
    fromMaybe True <$> runMaybeT (lookupInMap mp <|> lookupInCas)
  where
    !k = BloomKey a b

    lookupInMap mp = MaybeT (return $! HashMap.lookup k mp) >>= lookupInBloom

    lookupInCas = do
        !entry <- MaybeT (casLookup cas k)
        -- insert this lookup into cache because we were forced to load it from
        -- disk. It will be reaped at next cut update if necessary.
        liftIO $ modifyMVar_ mv $ \(!mp) ->
            return $! HashMap.insert (_beKey entry) entry mp
        lookupInBloom entry

    lookupInBloom = return . Bloom.elem h . _beBloom
{-# INLINE member #-}


------------------------------------------------------------------------------
waitForNewCut :: CutDb cas -> Maybe Cut -> IO Cut
waitForNewCut cutDb lastCut = atomically $ do
    !cut <- CutDB._cutStm cutDb
    when (lastCut == Just cut) retry
    return cut
{-# INLINE waitForNewCut #-}


-- TODO: configurable
-- | This parameter controls how many blocks back into history we'll keep in
-- ram.
mAX_HEIGHT_DELTA :: BlockHeight
mAX_HEIGHT_DELTA = 100


-- | Given a new cut, decodes all of the new block payloads and inserts the new
-- bloom entries into the CAS and in-memory cache.
update
    :: HasCallStack
    => PayloadCas cas
    => CutDb cas
    -> BloomCas
    -> [(ChainId, BlockHeaderDb)]
    -> MVar BloomMap
    -> Cut
    -> (BloomMap -> BloomMap)
    -> IO ()
update cutDb cas bdbs mv cut atEnd = modMap >>= casInsertBatch cas
  where
    -- Generates new bloom sketches, puts them in the in-memory map, and
    -- returns a vector of the new bloom map entries for insertion into the CAS
    -- store.
    modMap :: IO (V.Vector BloomEntry)
    modMap = modifyMVar mv $ \mp -> do
        (mp', entries) <- f mp
        !mp'' <- evaluate (atEnd mp')
        let !v = V.fromList entries
        return (mp'', v)

    f mp = foldM upd (mp, []) hdrs
    hdrs = HashMap.toList $ _cutMap cut
    upd (!mp, !entries) (cid, blockHeader) =
        updateChain cutDb cas (fromJuste $! lookup cid bdbs) blockHeader
                    mp entries
{-# INLINE update #-}


updateChain
    :: PayloadCas cas
    => CutDb cas
    -> BloomCas
    -> BlockHeaderDb
    -> BlockHeader
    -> BloomMap
    -> [BloomEntry]
    -> IO (BloomMap, [BloomEntry])
updateChain cutDb cas bdb blockHeader mp entries = do
    let leafHeight = _blockHeight blockHeader
    let minHeight = boundHeight leafHeight
    fromMaybe (mp, entries) <$!>
        runMaybeT (updateChain' cutDb cas bdb minHeight blockHeader mp entries)


boundHeight :: BlockHeight -> BlockHeight
boundHeight h | h <= mAX_HEIGHT_DELTA = 0
              | otherwise = h - mAX_HEIGHT_DELTA


updateChain'
    :: PayloadCas cas
    => CutDb cas
    -> BloomCas
    -> BlockHeaderDb
    -> BlockHeight
    -> BlockHeader
    -> BloomMap
    -> [BloomEntry]
    -> MaybeT IO (BloomMap, [BloomEntry])
updateChain' cutDb cas bdb minHeight = go
  where
    go !blockHeader !mp !entries = do
        b <- lookupHKey hkey mp
        if b
            then return (mp, entries)   -- we can stop here, rest of parents are in cache
            else do
                let bh = _blockHeight blockHeader
                (!mp', !entries') <- insBloom <|> return (mp, entries)
                if bh <= minHeight
                  then return (mp', entries')
                  else do
                    let parentHash = _blockParent blockHeader
                    parentHeader <- liftIO $ TreeDB.lookupM bdb parentHash
                    go parentHeader mp' entries'
      where
        hgt = _blockHeight blockHeader
        hc = _blockHash blockHeader
        hkey = BloomKey hgt hc
        insBloom = do
            let payloadHash = _blockPayloadHash blockHeader
            (PayloadWithOutputs txsBs _ _ _ _ _) <- MaybeT $ casLookup pdb payloadHash
            hashes <- mapM (fmap (H.toUntypedHash . _cmdHash) . fromTx) txsBs
            let (nbits, nh) = Bloom.suggestSizing (length hashes) bloomFalsePositiveRate
            let !bloom = Bloom.fromList (Bloom.cheapHashes nh) nbits $ toList hashes
            let !be = BloomEntry (BloomKey hgt hc) (fromIntegral nh) bloom
            let !mp' = HashMap.insert hkey be mp
            return $! (mp', be : entries)

    lookupHKey hkey mp = lookupMap hkey mp <|> lookupCas hkey
    lookupMap hkey mp = if HashMap.member hkey mp then pure True else empty
    lookupCas hkey = liftIO $ casMember cas hkey

    pdb = cutDb ^. CutDB.cutDbPayloadCas
    fromTx (tx, _) = MaybeT (return $! toPactTx tx)


bloomFalsePositiveRate :: Double
bloomFalsePositiveRate = 0.02


toPactTx :: Transaction -> Maybe (Command Text)
toPactTx (Transaction b) = decodeStrict' b
