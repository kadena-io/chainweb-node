{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Chainweb.Store.Git.Internal
-- Copyright: Copyright © 2018 - 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Gregory Collins <greg@kadena.io>, Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- Internal machineary for "Chainweb.Store.Git".

module Chainweb.Store.Git.Internal
  ( -- * Types
    -- ** Data
    GitStore(..)
  , GitStoreData(..)
  , TreeEntry(..)
  , LeafTreeData(..)
  , GitHash(..)
    -- ** Errors
  , GitFailure(..)

    -- * Queries
  , readLeafTree
  , readHeader
  , leaves'

    -- * Brackets
  , lockGitStore
  , withOid

    -- * Failure
    -- | Convenience functions for handling error codes returned from @libgit2@
    -- functions.
  , throwOnGitError
  , throwGitStoreFailure
  , maybeTGitError

    -- * Utils
  , getSpectrum
  , parseLeafTreeFileName
  , oidToByteString
  , getBlockHashBytes
  ) where

import qualified Bindings.Libgit2 as G

import Control.Concurrent.MVar (MVar, withMVar)
import Control.Error.Util (hoistMaybe, hush, nothing)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.Bits (complement, unsafeShiftL, (.&.))
import Data.Bytes.Get (runGetS)
import qualified Data.ByteString.Base64.URL as B64U
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as B
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import Data.Witherable (wither)

import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt, CSize)
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)

import UnliftIO.Exception (Exception, bracket, mask, throwIO)

-- internal modules

import Chainweb.BlockHash (BlockHash(..), BlockHashBytes(..))
import Chainweb.BlockHeader
    (BlockHeader, BlockHeight(..), decodeBlockHeader, decodeBlockHeight)

---

--------
-- TYPES
--------

-- | The fundamental git-based storage type. Can be initialized via
-- `withGitStore` and then queried as needed.
--
newtype GitStore = GitStore (MVar GitStoreData)

-- | Many of the functions in this module require this type. The easiest and
-- safest way to get it is via `lockGitStore`. Don't try and manipulate the
-- pointers yourself.
--
data GitStoreData = GitStoreData {
    _gitStore :: {-# UNPACK #-} !(Ptr G.C'git_repository)
  , _gitOdb :: {-# UNPACK #-} !(Ptr G.C'git_odb)
}

-- TODO It's almost certainly possible to give this an instance of `Storable`.
-- Then, the `_ltd_spectrum` field of `LeafTreeData` can become a Storable
-- Vector, from which the rest of the code can benefit.
--
-- See: https://github.com/fosskers/vectortiles/blob/ea1236a84a973e4b0517afeae903986736394a4b/lib/Geography/VectorTile/Geometry.hs#L44-L48
-- | A Haskell-friendly distillation of the `G.C'git_tree_entry` type.
--
data TreeEntry = TreeEntry {
    _te_blockHeight :: {-# UNPACK #-} !BlockHeight
  , _te_blockHash :: {-# UNPACK #-} !BlockHashBytes
  , _te_gitHash :: {-# UNPACK #-} !GitHash
} deriving (Show, Eq, Ord)

-- | While `TreeEntry` represents a kind of "pointer" to a stored `BlockHeader`,
-- `LeafTreeData` contains its "spectrum" (points to ancestors in the chain to
-- allow for fast traversal) and a further point to the actual blob data
-- containing the encoded `BlockHeader`.
--
data LeafTreeData = LeafTreeData {
    _ltd_treeEntry :: !TreeEntry
    -- ^ Pointer to the blob data associated with this `TreeEntry`. A
    -- `BlockHeader`.
  , _ltd_spectrum :: Vector TreeEntry
} deriving (Show)

-- | A reference to a particular git object, corresponding to the type
-- `G.C'git_oid`.
--
newtype GitHash = GitHash ByteString deriving (Eq, Ord, Show)

-- | See here for all possible libgit2 errors:
-- https://github.com/libgit2/libgit2/blob/99afd41f1c43c856d39e3b9572d7a2103875a771/include/git2/errors.h#L21
--
data GitFailure = GitFailure {
    gitFailureHFun :: Text  -- ^ The Haskell function the error was thrown from.
  , gitFailureCFun :: Text  -- ^ The C function that originated the error.
  , gitFailureErrorCode :: CInt
} deriving (Show)

instance Exception GitFailure

newtype GitStoreFailure = GitStoreFailure { gitStoreFailureReason :: Text }
  deriving (Show)

instance Exception GitStoreFailure

----------
-- QUERIES
----------

-- | Follow a (hopefully) established object id (`GitHash`) of a `TreeEntry` to
-- its data, yielding its "spectrum" and a further pointer to its blob data.
--
readLeafTree :: GitStoreData -> GitHash -> IO LeafTreeData
readLeafTree store treeGitHash = withTreeObject store treeGitHash readTree
  where
    readTree :: Ptr G.C'git_tree -> IO LeafTreeData
    readTree pTree = do
        numEntries <- G.c'git_tree_entrycount pTree
        elist <- traverse (readTreeEntry pTree) [0..(numEntries-1)]
        spectrum <- sortSpectrum elist
        when (V.null spectrum) $ throwGitStoreFailure "impossible: empty tree"
        let lastEntry = V.unsafeLast spectrum
        pure $! LeafTreeData lastEntry (V.init spectrum)

    readTreeEntry :: Ptr G.C'git_tree -> CSize -> IO TreeEntry
    readTreeEntry pTree idx = G.c'git_tree_entry_byindex pTree idx >>= fromTreeEntryP

    fromTreeEntryP :: Ptr G.C'git_tree_entry -> IO TreeEntry
    fromTreeEntryP entryP = do
        name <- G.c'git_tree_entry_name entryP >>= B.packCString
        oid  <- GitHash <$> (G.c'git_tree_entry_id entryP >>= oidToByteString)
        (h, bh) <- maybe (throwGitStoreFailure "Tree object with incorrect naming scheme!") pure
                         (parseLeafTreeFileName name)
        pure $! TreeEntry h bh oid

    sortSpectrum :: [TreeEntry] -> IO (Vector TreeEntry)
    sortSpectrum l = do
        mv <- V.unsafeThaw (V.fromList l)
        V.sort mv
        V.unsafeFreeze mv

-- | Fetch the `BlockHeader` that corresponds to some `TreeEntry`.
--
readHeader :: GitStoreData -> TreeEntry -> IO BlockHeader
readHeader store (TreeEntry _ _ gh) = do
    blobHash <- _te_gitHash . _ltd_treeEntry <$> readLeafTree store gh
    bs <- getBlob store blobHash
    either (throwGitStoreFailure . T.pack) pure $
        runGetS decodeBlockHeader bs

-- | Fetch the raw byte data of some object in the Git Store.
--
getBlob :: GitStoreData -> GitHash -> IO ByteString
getBlob (GitStoreData repo _) gh = bracket lookupBlob destroy readBlob
  where
    lookupBlob :: IO (Ptr G.C'git_blob)
    lookupBlob = mask $ \restore -> alloca $ \pBlob -> withOid gh $ \oid -> do
        throwOnGitError "getBlob" "git_blob_lookup" $
            restore $ G.c'git_blob_lookup pBlob repo oid
        peek pBlob

    destroy :: Ptr G.C'git_blob -> IO ()
    destroy = G.c'git_blob_free

    readBlob :: Ptr G.C'git_blob -> IO ByteString
    readBlob blob = do
        content <- G.c'git_blob_rawcontent blob
        size <- G.c'git_blob_rawsize blob
        B.packCStringLen (castPtr content, fromIntegral size)

-- | All leaf nodes in their light "pointer" form.
--
-- If we are pruning properly, there should only ever be a few of these, hence a
-- list is appropriate.
--
leaves' :: GitStoreData -> IO [TreeEntry]
leaves' (GitStoreData repo _) =
    withCString "leaf/*"
        $ \patt -> alloca
        $ \namesP -> do
            throwOnGitError "leaves" "git_tag_list_match" $
                G.c'git_tag_list_match namesP patt repo
            a <- peek namesP
            names <- peekArray (fromIntegral $ G.c'git_strarray'count a) (G.c'git_strarray'strings a)
            wither getEntry names  -- TODO Report malformed tag names instead of ignoring?
 where
   -- | Expected argument format:
   --
   -- @
   -- leaf/AAAAAAAAAAA=.7C1XaR2bLUAYKsVlAyBUt9eEupaxi8tb4LtOcOP7BB4=
   -- @
   --
   getEntry :: CString -> IO (Maybe TreeEntry)
   getEntry name = do
       name' <- B.packCString name
       let tagName = B.drop 5 name'  -- Slice off "leaf/"
           fullTagPath = B.concat [ "refs/tags/", name', "\0" ]
       B.unsafeUseAsCString fullTagPath
           $ \fullTagPath' -> alloca
           $ \oidP -> runMaybeT $ do
               (bh, bs) <- hoistMaybe $ parseLeafTreeFileName tagName
               maybeTGitError $ G.c'git_reference_name_to_id oidP repo fullTagPath'
               hash <- liftIO $ GitHash <$> oidToByteString oidP
               pure $! TreeEntry bh bs hash

-----------
-- BRACKETS
-----------

-- | Prevents other threads from manipulating the Git Store while we perform
-- some given action.
--
lockGitStore :: GitStore -> (GitStoreData -> IO a) -> IO a
lockGitStore (GitStore m) f = withMVar m f

-- | Bracket pattern around a `G.C'git_tree` struct.
--
withTreeObject
    :: GitStoreData
    -> GitHash
    -> (Ptr G.C'git_tree -> IO a)
    -> IO a
withTreeObject (GitStoreData repo _) gitHash f = bracket getTree G.c'git_tree_free f
  where
    getTree :: IO (Ptr G.C'git_tree)
    getTree = mask $ \restore -> alloca $ \ppTree -> withOid gitHash $ \oid -> do
        throwOnGitError "withTreeObject" "git_tree_lookup" $
            restore $ G.c'git_tree_lookup ppTree repo oid
        peek ppTree

withOid :: GitHash -> (Ptr G.C'git_oid -> IO a) -> IO a
withOid (GitHash strOid) f =
    B.unsafeUseAsCStringLen strOid $ \(cstr, clen) -> alloca $ \pOid -> do
        throwOnGitError "withOid" "git_oid_fromstrn" $
            G.c'git_oid_fromstrn pOid cstr (fromIntegral clen)
        f pOid

----------
-- FAILURE
----------

throwOnGitError :: Text -> Text -> IO CInt -> IO ()
throwOnGitError h c m = do
    code <- m
    when (code /= 0) $ throwGitError h c code

throwGitError :: Text -> Text -> CInt -> IO a
throwGitError h c e = throwIO $ GitFailure h c e

maybeTGitError :: IO CInt -> MaybeT IO ()
maybeTGitError m = do
    code <- liftIO m
    when (code /= 0) nothing

throwGitStoreFailure :: Text -> IO a
throwGitStoreFailure = throwIO . GitStoreFailure

--------
-- UTILS
--------

getSpectrum :: BlockHeight -> [BlockHeight]
getSpectrum (BlockHeight 0) = []
getSpectrum (BlockHeight d0) = map (BlockHeight . fromIntegral) . dedup $ startSpec ++ rlgs ++ recents
  where
    d0' :: Int64
    d0' = fromIntegral d0

    numRecents = 4
    d = max 0 (d0' - numRecents)
    recents = [d .. (max 0 (d0'-2))]       -- don't include d0 or its parent

    pow2s = [ 1 `unsafeShiftL` x | x <- [5..63] ]

    (startSpec, lastSpec) = fs id 0 pow2s
    diff = d - lastSpec

    -- reverse log spectrum should be quantized on the lower bits
    quantize :: Int64 -> Int64
    quantize !x = let !out = (d - x) .&. complement (x-1) in out

    lgs = map quantize $ takeWhile (< diff) pow2s
    rlgs = reverse lgs

    fs :: ([Int64] -> [Int64]) -> Int64 -> [Int64] -> ([Int64], Int64)
    fs !dl !lst (x:zs) | x < d     = fs (dl . (x:)) x zs
                       | otherwise = (dl [], lst)
    fs !dl !lst [] = (dl [], lst)

dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup o@[_] = o
dedup (x:r@(y:_)) | x == y = dedup r
                  | otherwise = x : dedup r

-- | Parse a git-object filename in the shape of:
--
-- @
-- 1023495.5e4fb6e0605385aee583035ae0db732e485715c8d26888d2a3571a26291fb58e
-- ^       ^
-- |       `-- base64-encoded block hash
-- `-- base64-encoded block height
-- @
parseLeafTreeFileName :: ByteString -> Maybe (BlockHeight, BlockHashBytes)
parseLeafTreeFileName fn = do
    height <- hush $ decodeHeight heightStr
    bh <- BlockHashBytes <$> hush (B64U.decode blockHash0)
    pure (height, bh)
  where
    -- TODO if the `rest` is fixed-length, it would be faster to use `splitAt`.
    (heightStr, rest) = B.break (== '.') fn
    blockHash0 = B.drop 1 rest

    decodeHeight :: ByteString -> Either String BlockHeight
    decodeHeight s = do
        s' <- B64U.decode s
        fromIntegral <$> runGetS decodeBlockHeight s'

oidToByteString :: Ptr G.C'git_oid -> IO ByteString
oidToByteString pOid = bracket (G.c'git_oid_allocfmt pOid) free B.packCString

-- | Mysteriously missing from the main API of `BlockHash`.
--
getBlockHashBytes :: BlockHash -> BlockHashBytes
getBlockHashBytes (BlockHash _ bytes) = bytes
