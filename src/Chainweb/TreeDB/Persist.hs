{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module: Chainweb.TreeDB.Persist
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- Reading a `TreeDb` from - or writing one to - the filesystem.

module Chainweb.TreeDB.Persist
  ( -- * Writing
    persist
    -- * Reading
  , fileEntries
  , restore
  ) where

import Control.Monad.Trans.Resource (MonadResource, MonadThrow(..), runResourceT)

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Streaming as BS
import qualified Data.ByteString.Streaming.Char8 as BS
import Data.Serialize (Serialize, encode, decode)

import Streaming
import qualified Streaming.Prelude as S

import System.Path (Path, Absolute, toFilePath)

-- internal modules

import Chainweb.TreeDB

-- -------------------------------------------------------------------------- --
-- TreeDb Persistence

-- | Persist the contents of some `TreeDb` to disk.
--
persist :: (TreeDb db, Serialize (DbEntry db)) => Path Absolute -> db -> IO ()
persist fp db = entries db Nothing Nothing Nothing Nothing $ \es ->
    runResourceT
        . BS.writeFile (toFilePath fp)
        . hoist lift
        . separated
        $ encoded $ void es

-- | Encode each `DbEntry` as a base64 `B.ByteString`.
--
encoded :: (Monad m, Serialize e) => Stream (Of e) m () -> Stream (Of B.ByteString) m ()
encoded = S.map (B64U.encode . encode)
{-# INLINE encoded #-}

-- | Form a ByteString stream from base64-encoded Entries, and divide them by
-- newline characters. A newline byte cannot appear in a base64 encodings, thus
-- making it a unique byte to split on.
--
separated :: Monad m => Stream (Of B.ByteString) m () -> BS.ByteString m ()
separated = BS.fromChunks . S.intersperse (B.singleton 0x0A)
{-# INLINE separated #-}

-- | Given a path to a database written to file via `persist` and a freshly
-- initialized `TreeDb` (i.e. it only contains the genesis block), update
-- the `TreeDb` to contain all entries in the file.
--
-- Throws exceptions if the path given doesn't exist,
-- or if certain ByteStrings within failed to decode.
--
-- /Note:/ This assumes that reinserting the genesis block into a `TreeDb`
-- that already contains it is a no-op. This invariant is enforced by tests.
--
restore :: (TreeDb db, Serialize (DbEntry db)) => Path Absolute -> db -> IO ()
restore fp db = runResourceT $ S.mapM_ (lift . insert db) $ fileEntries fp

-- | A stream of all entries, decoded from a file.
--
-- Throws exceptions if the path given doesn't exist,
-- or if certain ByteStrings within failed to decode.
--
fileEntries
    :: MonadThrow m
    => MonadResource m
    => Serialize e
    => Path Absolute
    -> Stream (Of e) m ()
fileEntries = decoded . destream . BS.lines . BS.readFile . toFilePath
{-# INLINE fileEntries #-}

-- | Flatten a "stream of streamable bytestrings". We know these are each
-- finite-sized encodings of `TreeDb` entries, which we don't expect to
-- be large.
--
destream :: Monad m => Stream (BS.ByteString m) m r -> Stream (Of B.ByteString) m r
destream = S.mapped BS.toStrict
{-# INLINE destream #-}

-- | Reverse the base64 encoding and further decode from our custom encoding.
-- Throws a `TreeDbException` if either decoding step fails.
--
decoded
    :: Monad m
    => Serialize e
    => Stream (Of B.ByteString) m r
    -> Stream (Of e) m r
decoded = S.mapM (\e -> either die pure $ B64U.decode e >>= decode)
  where
    -- TODO(colin): This should not just be `error`.
    die = error "Base-64 deserialization of persisted data failed."
{-# INLINE decoded #-}
