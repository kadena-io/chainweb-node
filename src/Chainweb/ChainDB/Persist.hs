{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

-- |
-- Module: Chainweb.ChainDB.Persist
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- Reading a `ChainDb` from - or writing one to - the filesystem.

module Chainweb.ChainDB.Persist
  ( -- * Writing
    dbEntries
  , persist
    -- * Reading
  , fileEntries
  , restore
  ) where

import Control.Concurrent.STM
import Control.Monad ((>=>))
import Control.Monad.Trans.Resource (MonadResource, MonadThrow(..), ResourceT, runResourceT)
import Control.Monad.Trans.State.Strict

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Streaming as BS
import qualified Data.ByteString.Streaming.Char8 as BS
import Data.Foldable (traverse_)

import Streaming
import qualified Streaming.Prelude as S

import System.Path (Path, Absolute, toFilePath)

-- internal modules

import Chainweb.ChainDB

-- -------------------------------------------------------------------------- --
-- ChainDb Persistence

persist :: Path Absolute -> ChainDb -> IO ()
persist (toFilePath -> fp) db =
    runResourceT . BS.writeFile fp . hoist lift . separated . encoded $ dbEntries db

-- | Given a `ChainDb`, stream all the Entries it contains in order of
-- block height, from oldest to newest.
--
-- Assumption: The first entry to be streamed is always the genesis block.
dbEntries :: ChainDb -> Stream (Of (Entry 'Checked)) IO ()
dbEntries db = lift (updates db) >>= \u -> lift (snapshot db) >>= f u
  where
    f !u !s = do
        e <- lift . atomically $ (Just <$> updatesNext u) `orElse` pure Nothing
        traverse_ (g u s) e
    g !u !s !k = lift (getEntrySync k s) >>= \(s', e) -> S.yield e >> f u s'

-- | Encode each `Entry` as a base64 `B.ByteString`.
encoded :: Monad m => Stream (Of (Entry 'Checked)) m () -> Stream (Of B.ByteString) m ()
encoded = S.map (B64.encode . encodeEntry)
{-# INLINE encoded #-}

-- | Form a ByteString stream from base64-encoded Entries, and divide them by
-- newline characters. A newline byte cannot appear in a base64 encodings, thus
-- making it a unique byte to split on.
separated :: Monad m => Stream (Of B.ByteString) m () -> BS.ByteString m ()
separated = BS.fromChunks . S.intersperse (B.singleton 0x0A)
{-# INLINE separated #-}

-- | Given a path to a database written to file via `persist` and a freshly
-- initialized `ChainDb` (i.e. it only contains the genesis block), update
-- the `ChainDb` to contain all entries in the file.
--
-- Throws exceptions if the path given doesn't exist,
-- or certain ByteStrings within failed to decode.
--
-- /Note:/ This assumes that reinserting the genesis block into a `ChainDb`
-- that already contains it is a no-op. This invariant is enforced by tests.
restore :: Path Absolute -> ChainDb -> IO ()
restore fp db = runResourceT $ do
    ss  <- lift $ snapshot db
    ss' <- execStateT (S.mapM_ goIn . hoist lift $ fileEntries fp) ss
    void . lift $ syncSnapshot ss'

-- | A stream of all entries, decoded from a file.
--
-- Throws exceptions if the path given doesn't exist,
-- or certain ByteStrings within failed to decode.
fileEntries
    :: MonadThrow m
    => MonadResource m
    => Path Absolute
    -> Stream (Of (Entry 'Unchecked)) m ()
fileEntries = decoded . destream . BS.lines . BS.readFile . toFilePath
{-# INLINE fileEntries #-}

goIn :: Entry s -> StateT Snapshot (ResourceT IO) ()
goIn e = get >>= insert e >>= put

-- | Flatten a "stream of streamable bytestrings". We know these are each
-- finite-sized encodings of `ChainDb` entries, which we don't expect to
-- be large.
destream :: Monad m => Stream (BS.ByteString m) m r -> Stream (Of B.ByteString) m r
destream = S.mapped BS.toStrict
{-# INLINE destream #-}

-- | Reverse the base64 encoding and further decode from our custom encoding.
-- Throws a `DbException` if either decoding step fails.
decoded :: MonadThrow m => Stream (Of B.ByteString) m r -> Stream (Of (Entry 'Unchecked)) m r
decoded = S.mapM (either die pure . B64.decode >=> decodeEntry)
  where
    die _ = throwM Base64DeserializationFailed
{-# INLINE decoded #-}
