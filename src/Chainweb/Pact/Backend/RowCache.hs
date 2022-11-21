{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Chainweb.Pact.Backend.RowCache
  ( RowCache
  , emptyRowCache
  , insertCache
  , gcCache
  , rewindCache
  , lookupCache
  , ModuleRowCache
  , TransactionalStore
  , mkTransactionalStore
  , beginStoreTx
  , commitStoreTx
  , rollbackStoreTx
  , withPendingStore
  , withCommittedStore
  , updateStore
  ) where


import Control.Monad
import Control.Monad.Catch

import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.HashSet as HS
import Database.SQLite3.Direct (Utf8(..))

import Pact.Types.Persistence (TxId,PersistModuleData)

-- | Rewindable row cache.
data RowCache k v = RowCache
    { -- | Row values indexed by key then txid.
      _rcRows :: HM.HashMap k (M.Map TxId v)
      -- | Index of txid to associated keys
    , _rcIndex :: M.Map TxId (HS.HashSet k)
    }
    deriving (Eq,Show)

emptyRowCache :: (Eq k, Hashable k) => RowCache k v
emptyRowCache = RowCache mempty mempty


-- | Cache insert at txid.
insertCache :: (Eq k, Hashable k) => TxId -> k -> v -> RowCache k v -> RowCache k v
insertCache tid k v (RowCache rs idx) = RowCache
    (HM.insertWith (M.union) k (M.singleton tid v) rs)
    (M.insertWith (HS.union) tid (HS.singleton k) idx)

-- | Cache garbage collect, deleting any entries before the txid for keys
-- that have values at or after the txid. After this operation, attempting
-- to rewind before the GC point will result in undefined behavior.
gcCache :: (Eq k, Hashable k) => TxId -> RowCache k v -> RowCache k v
gcCache tid (RowCache rs idx) = RowCache rs' idxAfter
  where
    idxAfter = M.dropWhileAntitone (< tid) idx
    keysAfter = HS.unions $ M.elems idxAfter
    rs' = HS.foldl' (\r k -> HM.update go k r) rs keysAfter
    go = Just . M.dropWhileAntitone (< tid)

-- | Cache rewind, deleting all values after txid.
rewindCache :: (Eq k, Hashable k) => TxId -> RowCache k v -> RowCache k v
rewindCache tid (RowCache rs idx) = RowCache rs' idxKeep
  where
    (idxKeep,idxDrop) = M.spanAntitone (<= tid) idx
    keysDrop = HS.unions $ M.elems idxDrop
    rs' = HS.foldl' (\r k -> HM.update go k r) rs keysDrop
    go = Just . M.takeWhileAntitone (<= tid)

-- | Cache lookup, returning latest txid version of value.
lookupCache :: (Eq k, Hashable k) => k -> RowCache k v -> Maybe v
lookupCache k (RowCache c _) = (HM.lookup k >=> fmap snd . M.lookupMax) c

-- | Module row cache with filtering. A 'Nothing' value means
-- that the cache is intentionally empty for a key as of a given txid,
-- resulting in a cache miss/read from db. This handles the
-- case that a module is included but then excluded after an upgrade,
-- where we want the cache to "intentionally miss" instead of
-- incorrectly returning the previous value.
type ModuleRowCache = RowCache Utf8 (Maybe PersistModuleData)

-- | Orphan that delegates to bytestring.
instance Hashable Utf8 where
  hashWithSalt i (Utf8 s) = hashWithSalt i s

-- | In-memory transactional store.
-- Private constructors to enforce correct access
data TransactionalStore a =
  -- | Committed singleton state
  Committed a |
  -- | Pending (committed,pending) state
  -- Committed inaccessible, only used for rollback.
  Pending a a

data TransactionalStoreError =
  TransactionalStoreErrorInvalidState
  deriving (Eq,Show)
instance Exception TransactionalStoreError

beginStoreTx :: MonadThrow m => TransactionalStore a -> m (TransactionalStore a)
beginStoreTx = withCommittedStore $ \c -> pure $! Pending c c

commitStoreTx :: MonadThrow m => TransactionalStore a -> m (TransactionalStore a)
commitStoreTx = withPendingStore $ \p -> pure $! Committed p

rollbackStoreTx :: MonadThrow m => TransactionalStore a -> m (TransactionalStore a)
rollbackStoreTx Committed {} = throwM TransactionalStoreErrorInvalidState
rollbackStoreTx (Pending c _) = pure $! Committed c

withPendingStore :: MonadThrow m => (a -> m b) -> TransactionalStore a -> m b
withPendingStore _ Committed {} = throwM TransactionalStoreErrorInvalidState
withPendingStore f (Pending _ p) = f p

withCommittedStore :: MonadThrow m => (a -> m b) -> TransactionalStore a -> m b
withCommittedStore _ Pending {} = throwM TransactionalStoreErrorInvalidState
withCommittedStore f (Committed c) = f c

updateStore :: MonadThrow m => (a -> m a) -> TransactionalStore a -> m (TransactionalStore a)
updateStore _ Committed {} = throwM TransactionalStoreErrorInvalidState
updateStore f (Pending c p) = f p >>= \p' -> pure $! Pending c p'

mkTransactionalStore :: a -> TransactionalStore a
mkTransactionalStore a = Committed a
