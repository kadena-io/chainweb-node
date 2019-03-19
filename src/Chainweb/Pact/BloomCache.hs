{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | This structure records a probabilistic sketch of which transactions were
-- processed in recent blocks. We keep a (fixed) maximum block horizon, i.e. if
--
--   h = min_height_in_cut,
--
-- the cache will not contain sketches for blocks smaller than h - d. Currently
-- d = 16384, meaning we'll spend approximately 4-8MB per chain for this cache
-- if the blocks average 100 txs (blooms will be ~128 bytes plus data + hashmap
-- overhead in this case).
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
import Control.Concurrent.STM
import Control.Exception
import Control.Lens ((^.))
import Control.Monad (foldM, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.BloomFilter (Bloom)
import qualified Data.BloomFilter as Bloom
import qualified Data.BloomFilter.Easy as Bloom
import Data.ByteString.Char8 (ByteString)
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Pact.Types.Command
import Pact.Types.Util (Hash(..))
------------------------------------------------------------------------------
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.Cut
import Chainweb.CutDB (CutDb)
import qualified Chainweb.CutDB as CutDB
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import qualified Chainweb.TreeDB as TreeDB
import Data.CAS
------------------------------------------------------------------------------

type TransactionBloomCache_ = HashMap (BlockHeight, BlockHash) (Bloom ByteString)

data TransactionBloomCache = TransactionBloomCache {
    _map :: {-# UNPACK #-} !(IORef TransactionBloomCache_)
  , _thread :: {-# UNPACK #-} !(Async ())
}

createCache
    :: PayloadCas cas
    => CutDb cas
    -> BlockHeaderDb
    -> IO TransactionBloomCache
createCache cutDb bdb = mask_ $ do
    !m <- newIORef HashMap.empty
    !t <- Async.asyncWithUnmask $ threadProc m
    return $! TransactionBloomCache m t

  where
    threadProc mapVar restore = begin
      where
        begin = silently . restore $ go Nothing
        go !lastCut = do
            !cut <- waitForNewCut cutDb lastCut
            update cutDb bdb mapVar cut (reap $! lowestHeight cut)
            go $! Just cut
        silently = flip catches [
                -- cancellation msg should quit, all others restart
                Handler (\(_::SomeAsyncException) -> return ())
              , Handler (\(_::SomeException) -> begin)
              ]

    lowestHeight = foldl' f (BlockHeight maxBound) . HashMap.toList . _cutMap
      where
        f bh (_, hdr) = min bh $ _blockHeight hdr

    reap !minHeight = HashMap.filterWithKey $ const . (>= minHeight) . fst


destroyCache :: TransactionBloomCache -> IO ()
destroyCache = Async.cancel . _thread


withCache
    :: PayloadCas cas
    => CutDb cas
    -> BlockHeaderDb
    -> (TransactionBloomCache -> IO a)
    -> IO a
withCache cutDb bdb = bracket (createCache cutDb bdb) destroyCache


member :: Hash -> (BlockHeight, BlockHash) -> TransactionBloomCache -> IO Bool
member (Hash h) k (TransactionBloomCache mv _) = do
    mp <- readIORef mv
    -- N.B. return false positive on block missing
    fmap (fromMaybe True) $ runMaybeT $ do
        !bloom <- MaybeT $ return $! HashMap.lookup k mp
        return $! Bloom.elem h bloom


------------------------------------------------------------------------------
waitForNewCut :: CutDb cas -> Maybe Cut -> IO Cut
waitForNewCut cutDb lastCut = atomically $ do
    !cut <- CutDB._cutStm cutDb
    when (lastCut == Just cut) retry
    return cut


-- TODO: configurable
mAX_HEIGHT_DELTA :: BlockHeight
mAX_HEIGHT_DELTA = 16384


update
    :: PayloadCas cas
    => CutDb cas
    -> BlockHeaderDb
    -> IORef TransactionBloomCache_
    -> Cut
    -> (TransactionBloomCache_ -> TransactionBloomCache_)
    -> IO ()
update cutDb bdb mv cut atEnd = do
    mp <- readIORef mv
    !mp' <- atEnd <$> f mp
    writeIORef mv mp'
  where
    f mp = foldM upd mp hdrs
    hdrs = HashMap.toList $ _cutMap cut
    upd !mp (_, blockHeader) = updateChain cutDb bdb blockHeader mp


updateChain
    :: PayloadCas cas
    => CutDb cas
    -> BlockHeaderDb
    -> BlockHeader
    -> TransactionBloomCache_
    -> IO TransactionBloomCache_
updateChain cutDb bdb blockHeader mp = do
    let leafHeight = _blockHeight blockHeader
    let minHeight = boundHeight leafHeight
    fromMaybe mp <$> runMaybeT (updateChain' cutDb bdb minHeight blockHeader mp)
  where
    boundHeight h | h <= mAX_HEIGHT_DELTA = 0
                  | otherwise = h - mAX_HEIGHT_DELTA


updateChain'
    :: PayloadCas cas
    => CutDb cas
    -> BlockHeaderDb
    -> BlockHeight
    -> BlockHeader
    -> TransactionBloomCache_
    -> MaybeT IO TransactionBloomCache_
updateChain' cutDb bdb minHeight blockHeader0 mp0 = go mp0 blockHeader0
  where
    go !mp !blockHeader =
        if HashMap.member hkey mp
            then return mp   -- we can stop here, rest of parents are in cache
            else do
                !mp' <- insBloom <|> return mp
                let parentHash = _blockParent blockHeader
                parentHeader <- liftIO $ TreeDB.lookupM bdb parentHash
                let parentHeight = _blockHeight parentHeader
                if parentHeight <= minHeight
                    then return mp'
                    else go mp' parentHeader
      where
        hgt = _blockHeight blockHeader
        hc = _blockHash blockHeader
        hkey = (hgt, hc)
        insBloom = do
            let payloadHash = _blockPayloadHash blockHeader
            (PayloadWithOutputs txsBs _ _ _) <- MaybeT $ casLookup pdb payloadHash
            hashes <- mapM (fmap (unH . _cmdHash) . fromTx) txsBs
            let !bloom = Bloom.easyList 0.025 $ toList hashes
            return $! HashMap.insert hkey bloom mp

    pdb = cutDb ^. CutDB.cutDbPayloadCas
    fromTx (tx, _) = MaybeT (return (toPactTx tx))
    unH (Hash h) = h


toPactTx :: Transaction -> Maybe (Command Text)
toPactTx (Transaction b) = decodeStrict b

