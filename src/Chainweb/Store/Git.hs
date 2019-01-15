{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module: Chainweb.Store.Git
-- Copyright: Copyright © 2018 - 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Gregory Collins <greg@kadena.io>, Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- Git-based `BlockHeader` storage.

module Chainweb.Store.Git
  ( -- * Types
    GitStoreConfig(..)
  , GitStore
  , GitFailure(..)

  -- * Initialization
  , withGitStore

  -- * Insertion
  , InsertResult(..)
  , insertBlock

  -- * Lookup
  , lookupByBlockHash
  , leaves

  -- * Utilities
  -- , lockGitStore
  , getSpectrum
  , parseLeafTreeFileName
  ) where

import qualified Bindings.Libgit2 as G

import Control.Concurrent.MVar
import Control.Error.Util (hoistMaybe, hush, nothing)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.Bits (complement, unsafeShiftL, (.&.), (.|.))
import Data.Bool (bool)
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import qualified Data.ByteString.Base64.URL as B64U
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as B
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import Data.Witherable (wither)

import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt, CSize, CUInt)
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)

import Prelude hiding (lookup)

import System.Path (Absolute, Path, toFilePath)

import UnliftIO.Exception
    (Exception, bracket, bracketOnError, bracket_, finally, mask, throwIO)

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader

---

-- | For opening an existing repository, or for initializing a new one.
--
data GitStoreConfig = GitStoreConfig {
    _gsc_path :: !(Path Absolute)
  , _gsc_genesis :: !BlockHeader
}

data GitStoreData = GitStoreData {
    _gitStore :: {-# UNPACK #-} !(Ptr G.C'git_repository)
  , _gitOdb :: {-# UNPACK #-} !(Ptr G.C'git_odb)
}

-- TODO So, this is to have the instance of `TreeDb`?
-- | The fundamental git-based storage type. Can be initialized via
-- `withGitStore` and then queried as needed.
--
newtype GitStore = GitStore (MVar GitStoreData)

-- | A reference to a particular git object.
--
newtype GitHash = GitHash ByteString deriving (Eq, Ord, Show)

-- TODO It's almost certainly possible to give this an instance of `Storable`.
-- Then, the `_ltd_spectrum` field of `LeafTreeData` can become a Storable
-- Vector, from which the rest of the code can benefit.
--
-- See: https://github.com/fosskers/vectortiles/blob/ea1236a84a973e4b0517afeae903986736394a4b/lib/Geography/VectorTile/Geometry.hs#L44-L48
data TreeEntry = TreeEntry {
    _te_blockHeight :: {-# UNPACK #-} !BlockHeight
  , _te_blockHash :: {-# UNPACK #-} !BlockHashBytes
  , _te_gitHash :: {-# UNPACK #-} !GitHash
} deriving (Show, Eq, Ord)

data LeafTreeData = LeafTreeData {
    _ltd_treeEntry :: !TreeEntry
  , _ltd_spectrum :: Vector TreeEntry
} deriving (Show)

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

lockGitStore :: GitStore -> (GitStoreData -> IO a) -> IO a
lockGitStore (GitStore m) f = withMVar m f

-- | A bracket-pattern that gives access to a `GitStore`, the fundamental
-- git-based storage type. Given a `GitStoreConfig`, this function first assumes
-- an existing repository, and tries to open it. If no such repository exists,
-- it will create a fresh one with the provided genesis `BlockHeader`.
--
-- Low-level pointers to the underlying git repository are freed automatically.
--
withGitStore :: GitStoreConfig -> (GitStore -> IO a) -> IO a
withGitStore (GitStoreConfig root0 g) f = G.withLibGitDo $ bracket open close f
  where
    root :: String
    root = toFilePath root0

    open :: IO GitStore
    open = mask $ \restore ->
        bracketOnError (openRepo restore) G.c'git_repository_free $ \repo -> do
            odb <- openOdb restore repo
            let gsd = GitStoreData repo odb
            insertGenesisBlock g gsd  -- INVARIANT: Should be a no-op for an existing repo.
            GitStore <$> newMVar gsd

    close :: GitStore -> IO ()
    close m = lockGitStore m $ \(GitStoreData p o) ->
        G.c'git_odb_free o `finally` G.c'git_repository_free p

    openRepo :: (IO CInt -> IO CInt) -> IO (Ptr G.C'git_repository)
    openRepo restore = tryOpen restore >>= maybe (initBare restore) pure

    -- | Attempt to open a supposedly existing git store, and fail gracefully if
    -- it doesn't.
    --
    tryOpen :: (IO CInt -> IO CInt) -> IO (Maybe (Ptr G.C'git_repository))
    tryOpen restore = withCString root $ \rootPtr -> alloca $ \repoPtr -> do
        res <- restore $ G.c'git_repository_open_ext repoPtr rootPtr openFlags rootPtr
        bool (pure Nothing) (Just <$> peek repoPtr) $ res == 0

    -- | Initialize an empty git store.
    --
    initBare :: (IO CInt -> IO CInt) -> IO (Ptr G.C'git_repository)
    initBare restore = withCString root $ \rootPtr -> alloca $ \repoPtr -> do
        throwOnGitError "initBare" "git_repository_init" . restore $
            G.c'git_repository_init repoPtr rootPtr 1
        peek repoPtr

    openOdb :: (IO () -> IO a) -> Ptr G.C'git_repository -> IO (Ptr G.C'git_odb)
    openOdb restore repo = alloca $ \podb -> do
        void . restore . throwOnGitError "openOdb" "git_repository_odb" $
            G.c'git_repository_odb podb repo
        peek podb

    -- not sure why this flag is not in gitlab2 bindings
    _FLAG_OPEN_BARE :: CUInt
    _FLAG_OPEN_BARE = 4

    openFlags :: CUInt
    openFlags = G.c'GIT_REPOSITORY_OPEN_NO_SEARCH .|. _FLAG_OPEN_BARE


------------------------------------------------------------------------------
data InsertResult = Inserted | AlreadyExists deriving (Eq, Show)

insertBlock :: GitStore -> BlockHeader -> IO InsertResult
insertBlock gs bh = lockGitStore gs $ \store -> do
    let hash = getBlockHashBytes $ _blockHash bh
        height = fromIntegral $ _blockHeight bh
    m <- lookupTreeEntryByHash store hash height
    maybe (go store) (const $ pure AlreadyExists) m
  where
    go :: GitStoreData -> IO InsertResult
    go store = createLeafTree store bh $> Inserted


------------------------------------------------------------------------------
lookupByBlockHash :: GitStore -> BlockHeight -> BlockHash -> IO (Maybe BlockHeader)
lookupByBlockHash gs height bh = lockGitStore gs $ \store -> do
    m <- lookupTreeEntryByHash store (getBlockHashBytes bh) (fromIntegral height)
    traverse (readHeader store) m
  where

-- | Fetch the `BlockHeader` that corresponds to some `TreeEntry`.
--
readHeader :: GitStoreData -> TreeEntry -> IO BlockHeader
readHeader store (TreeEntry _ _ gh) = do
    blobHash <- _te_gitHash . _ltd_treeEntry <$> readLeafTree store gh
    bs <- getBlob store blobHash
    either (throwGitStoreFailure . T.pack) pure $
        runGetS decodeBlockHeader bs


------------------------------------------------------------------------------
-- TODO The contents of @.git/refs/tag/leaf/@ also needs to be manually cleared.
-- collectGarbage :: GitStore -> IO ()
-- collectGarbage _ = pure $! () -- TODO


------------------------------------------------------------------------------
insertBlockHeaderIntoOdb :: GitStoreData -> BlockHeader -> IO GitHash
insertBlockHeaderIntoOdb (GitStoreData _ odb) bh =
    B.unsafeUseAsCStringLen serializedBlockHeader write
  where
    !serializedBlockHeader = runPutS $! encodeBlockHeader bh

    write :: (Ptr a, Int) -> IO GitHash
    write (cs, len) = alloca $ \oidPtr -> do
       throwOnGitError "insertBlockHeaderIntoOdb" "git_odb_write" $
           G.c'git_odb_write oidPtr odb (castPtr cs)
                                        (fromIntegral len)
                                        G.c'GIT_OBJ_BLOB
       GitHash <$> oidToByteString oidPtr


------------------------------------------------------------------------------
getBlockHashBytes :: BlockHash -> BlockHashBytes
getBlockHashBytes (BlockHash _ bytes) = bytes


------------------------------------------------------------------------------
-- | Insert a tree entry into a @git_treebuilder@.
--
tbInsert
    :: Ptr G.C'git_treebuilder
    -> G.C'git_filemode_t
    -> BlockHeight
    -> BlockHashBytes
    -> GitHash
    -> IO ()
tbInsert tb mode h hs gh =
    withOid gh $ \oid ->
    B.unsafeUseAsCString (terminate name) $ \cname ->
    throwOnGitError "tbInsert" "git_treebuilder_insert" $
        G.c'git_treebuilder_insert nullPtr tb cname oid mode
  where
    name :: NullTerminated
    name = mkTreeEntryNameWith "" h hs


------------------------------------------------------------------------------
-- | Given a block header: lookup its parent leaf tree entry, write the block
-- header into the object database, compute a spectrum for the new tree entry,
-- write the @git_tree@ to the repository, tag it under @tags/bh/foo@, and
-- returns the git hash of the new @git_tree@ object.
createLeafTree :: GitStoreData -> BlockHeader -> IO GitHash
createLeafTree store@(GitStoreData repo _) bh = withTreeBuilder $ \treeB -> do
    when (height <= 0) $ throwGitStoreFailure "cannot insert genesis block"
    parentTreeEntry <- lookupTreeEntryByHash store parentHash (height - 1) >>=
                       maybe (throwGitStoreFailure "parent hash not found in DB") pure
    let parentTreeGitHash = _te_gitHash parentTreeEntry
    parentTreeData <- readLeafTree store parentTreeGitHash
    treeEntries <- traverse (\h -> lookupTreeEntryByHeight' store parentTreeGitHash h parentTreeData)
                        spectrum
    newHeaderGitHash <- insertBlockHeaderIntoOdb store bh
    traverse_ (addTreeEntry treeB) treeEntries
    addTreeEntry treeB parentTreeEntry
    addSelfEntry treeB height hash newHeaderGitHash
    treeHash <- alloca $ \oid -> do
        throwOnGitError "createLeafTree" "git_treebuilder_write" $
            G.c'git_treebuilder_write oid repo treeB
        GitHash <$> oidToByteString oid
    createBlockHeaderTag store bh treeHash

    updateLeafTags store parentTreeEntry (TreeEntry height hash treeHash)

    -- TODO:
    --   - compute total difficulty weight vs the winning block, and atomic-replace
    --     the winning ref (e.g. @tags/BEST@) if the new block is better
    pure treeHash

  where
    height :: BlockHeight
    height = _blockHeight bh

    hash :: BlockHashBytes
    hash = getBlockHashBytes $ _blockHash bh

    parentHash :: BlockHashBytes
    parentHash = getBlockHashBytes $ _blockParent bh

    spectrum :: [BlockHeight]
    spectrum = getSpectrum height

    addTreeEntry :: Ptr G.C'git_treebuilder -> TreeEntry -> IO ()
    addTreeEntry tb (TreeEntry h hs gh) = tbInsert tb G.c'GIT_FILEMODE_TREE h hs gh

addSelfEntry :: Ptr G.C'git_treebuilder -> BlockHeight -> BlockHashBytes -> GitHash -> IO ()
addSelfEntry tb h hs gh = tbInsert tb G.c'GIT_FILEMODE_BLOB h hs gh


------------------------------------------------------------------------------
-- | Create a tag within @.git/refs/tags/bh/@ that matches the
-- @blockheight.blockhash@ syntax, as say found in a stored `BlockHeader`'s
-- "spectrum".
--
createBlockHeaderTag :: GitStoreData -> BlockHeader -> GitHash -> IO ()
createBlockHeaderTag gs@(GitStoreData repo _) bh leafHash =
    withObject gs leafHash $ \obj ->
    alloca $ \pTagOid ->
    B.unsafeUseAsCString (terminate tagName) $ \cstr ->
    -- @1@ forces libgit to overwrite this tag, should it already exist.
    throwOnGitError "createBlockHeaderTag" "git_tag_create_lightweight" $
        G.c'git_tag_create_lightweight pTagOid repo cstr obj 1
  where
    height :: BlockHeight
    height = _blockHeight bh

    hash :: BlockHashBytes
    hash = getBlockHashBytes $ _blockHash bh

    tagName :: NullTerminated
    tagName = mkTagName height hash


------------------------------------------------------------------------------
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


------------------------------------------------------------------------------
withOid :: GitHash -> (Ptr G.C'git_oid -> IO a) -> IO a
withOid (GitHash strOid) f =
    B.unsafeUseAsCStringLen strOid $ \(cstr, clen) -> alloca $ \pOid -> do
        throwOnGitError "withOid" "git_oid_fromstrn" $
            G.c'git_oid_fromstrn pOid cstr (fromIntegral clen)
        f pOid


------------------------------------------------------------------------------
withObject :: GitStoreData -> GitHash -> (Ptr G.C'git_object -> IO a) -> IO a
withObject (GitStoreData repo _) hash f =
    withOid hash $ \oid ->
    alloca $ \pobj -> do
        throwOnGitError "withObject" "git_object_lookup" $
            G.c'git_object_lookup pobj repo oid G.c'GIT_OBJ_ANY
        peek pobj >>= f


------------------------------------------------------------------------------

-- | Fetch the raw byte data of some object in the Git Store.
--
getBlob :: GitStoreData -> GitHash -> IO ByteString
getBlob (GitStoreData repo _) gh = bracket lookup destroy readBlob
  where
    lookup :: IO (Ptr G.C'git_blob)
    lookup = mask $ \restore -> alloca $ \pBlob -> withOid gh $ \oid -> do
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


------------------------------------------------------------------------------
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


------------------------------------------------------------------------------
{-
withReference :: GitStoreData
              -> ByteString
              -> (Ptr G.C'git_reference -> IO a)    -- ^ ptr may be null
              -> IO a
withReference (GitStoreData repo _) path0 f = bracket lookup destroy f
  where
    path :: ByteString
    path = B.append "refs/" path0

    destroy :: Ptr G.C'git_reference -> IO ()
    destroy p = when (p /= nullPtr) $ G.c'git_reference_free p

    lookup :: IO (Ptr G.C'git_reference)
    lookup = mask $ \restore ->
             alloca $ \pRef ->
             B.unsafeUseAsCString path $ \cstr -> do
        code <- restore $ G.c'git_reference_lookup pRef repo cstr
        if | code == G.c'GIT_ENOTFOUND -> pure nullPtr
           | code /= 0 -> throwGitError code
           | otherwise -> peek pRef
-}


------------------------------------------------------------------------------
withTreeBuilder :: (Ptr G.C'git_treebuilder -> IO a) -> IO a
withTreeBuilder f =
    alloca $ \pTB -> bracket_ (make pTB)
                              (peek pTB >>= G.c'git_treebuilder_free)
                              (peek pTB >>= f)
  where
    make :: Ptr (Ptr G.C'git_treebuilder) -> IO ()
    make p = throwOnGitError "withTreeBuilder" "git_treebuilder_create" $
        G.c'git_treebuilder_create p nullPtr


------------------------------------------------------------------------------
oidToByteString :: Ptr G.C'git_oid -> IO ByteString
oidToByteString pOid = bracket (G.c'git_oid_allocfmt pOid) free B.packCString


------------------------------------------------------------------------------
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
        pure $! LeafTreeData lastEntry (V.take (V.length spectrum - 1) spectrum)

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


------------------------------------------------------------------------------
-- | The parent node upon which our new node was written is by definition no
-- longer a leaf, and thus its entry in @.git/refs/leaf/@ must be removed.
--
updateLeafTags :: GitStoreData -> TreeEntry -> TreeEntry -> IO ()
updateLeafTags store@(GitStoreData repo _) oldLeaf newLeaf = do
    tagAsLeaf store newLeaf
    B.unsafeUseAsCString (terminate $ mkName oldLeaf) $ \cstr ->
        throwOnGitError "updateLeafTags" "git_tag_delete" $
            G.c'git_tag_delete repo cstr

-- | Tag a `TreeEntry` in @.git/refs/leaf/@.
--
tagAsLeaf :: GitStoreData -> TreeEntry -> IO ()
tagAsLeaf store@(GitStoreData repo _) leaf =
    withObject store (_te_gitHash leaf) $ \obj ->
        alloca $ \pTagOid ->
        B.unsafeUseAsCString (terminate $ mkName leaf) $ \cstr ->
        throwOnGitError "tagAsLeaf" "git_tag_create_lightweight" $
            G.c'git_tag_create_lightweight pTagOid repo cstr obj 1

mkName :: TreeEntry -> NullTerminated
mkName (TreeEntry h bh _) = mkLeafTagName h bh

------------------------------------------------------------------------------
-- | A `ByteString` whose final byte is a @\0@. Don't try to be too clever with
-- this.
--
newtype NullTerminated = NullTerminated { _unterminated :: [ByteString] }

nappend :: ByteString -> NullTerminated -> NullTerminated
nappend b (NullTerminated b') = NullTerminated $ b : b'
{-# INLINE nappend #-}

terminate :: NullTerminated -> ByteString
terminate = B.concat . _unterminated
{-# INLINE terminate #-}

-- | Encode a `BlockHeight` and `BlockHashBytes` into the expected format,
-- append some decorator to the front (likely a section of a filepath), and
-- postpend a null-terminator.
--
mkTreeEntryNameWith :: ByteString -> BlockHeight -> BlockHashBytes -> NullTerminated
mkTreeEntryNameWith b height hash = NullTerminated [ b, encHeight, ".", encBH, "\0" ]
  where
    encBH = B64U.encode $! runPutS (encodeBlockHashBytes hash)
    encHeight = B64U.encode $! runPutS (encodeBlockHeight height)

mkTagName :: BlockHeight -> BlockHashBytes -> NullTerminated
mkTagName = mkTreeEntryNameWith "bh/"

mkLeafTagName :: BlockHeight -> BlockHashBytes -> NullTerminated
mkLeafTagName = mkTreeEntryNameWith "leaf/"

mkTagRef :: BlockHeight -> BlockHashBytes -> NullTerminated
mkTagRef height hash = nappend "tags/" (mkTagName height hash)


------------------------------------------------------------------------------
lookupRefTarget
    :: GitStoreData
    -> NullTerminated      -- ^ ref path, e.g. tags/foo
    -> IO (Maybe GitHash)
lookupRefTarget (GitStoreData repo _) path0 =
    B.unsafeUseAsCString (terminate path)
        $ \cpath -> alloca
        $ \pOid -> runMaybeT $ do
            maybeTGitError $ G.c'git_reference_name_to_id pOid repo cpath
            GitHash <$> liftIO (oidToByteString pOid)
  where
    path :: NullTerminated
    path = nappend "refs/" path0


------------------------------------------------------------------------------
lookupTreeEntryByHash
    :: GitStoreData
    -> BlockHashBytes
    -> BlockHeight
    -> IO (Maybe TreeEntry)
lookupTreeEntryByHash gs bh height =
    fmap (TreeEntry height bh) <$> lookupRefTarget gs tagRef
  where
    tagRef :: NullTerminated
    tagRef = mkTagRef height bh


------------------------------------------------------------------------------
lookupTreeEntryByHeight
    :: GitStoreData
    -> GitHash         -- ^ starting from this leaf tree
    -> BlockHeight     -- ^ desired blockheight
    -> IO TreeEntry
lookupTreeEntryByHeight gs leafTreeHash height =
    readLeafTree gs leafTreeHash >>=
    lookupTreeEntryByHeight' gs leafTreeHash height


------------------------------------------------------------------------------
lookupTreeEntryByHeight'
    :: GitStoreData
    -> GitHash
    -> BlockHeight     -- ^ desired blockheight
    -> LeafTreeData
    -> IO TreeEntry
lookupTreeEntryByHeight' gs leafTreeHash height (LeafTreeData (TreeEntry leafHeight leafBH _) spectrum)
    | height == leafHeight = pure $! TreeEntry height leafBH leafTreeHash
    | V.null spec' = throwGitStoreFailure "lookup failure"
    | otherwise = search
  where
    spec' :: Vector TreeEntry
    spec' = V.filter (\t -> _te_blockHeight t >= height) spectrum

    search :: IO TreeEntry
    search = do
        let first = V.unsafeHead spec'
            gh = _te_gitHash first
        if | _te_blockHeight first == height -> pure first
           | otherwise -> lookupTreeEntryByHeight gs gh height


------------------------------------------------------------------------------
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


------------------------------------------------------------------------------
_isSorted :: Ord a => [a] -> Bool
_isSorted [] = True
_isSorted [_] = True
_isSorted (x:z@(y:_)) = x < y && _isSorted z


_prop_spectra_sorted :: Bool
_prop_spectra_sorted = all _isSorted $ map getSpectrum [1,10000000 :: BlockHeight]


------------------------------------------------------------------------------
dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup o@[_] = o
dedup (x:r@(y:_)) | x == y = dedup r
                  | otherwise = x : dedup r


------------------------------------------------------------------------------
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

------------------------------------------------------------------------------
throwGitStoreFailure :: Text -> IO a
throwGitStoreFailure = throwIO . GitStoreFailure


------------------------------------------------------------------------------
-- | A simplified version of `createLeafTree`, specialized for the Genesis
-- Block.
--
insertGenesisBlock :: BlockHeader -> GitStoreData -> IO ()
insertGenesisBlock g store@(GitStoreData repo _) = withTreeBuilder $ \treeB -> do
    newHeaderGitHash <- insertBlockHeaderIntoOdb store g
    addSelfEntry treeB (_blockHeight g) (getBlockHashBytes $ _blockHash g) newHeaderGitHash
    treeHash <- alloca $ \oid -> do
        throwOnGitError "insertGenesisBlock" "git_treebuilder_write" $
            G.c'git_treebuilder_write oid repo treeB
        GitHash <$> oidToByteString oid
    -- Store a tag in @.git/refs/tags/bh/@
    createBlockHeaderTag store g treeHash
    -- Mark this entry (it's the only entry!) as a "leaf" in @.git/refs/tags/leaf/@.
    tagAsLeaf store (TreeEntry 0 (getBlockHashBytes $ _blockHash g) treeHash)

------------------------------------------------------------------------------
-- | The "leaves" - the tips of all branches.
--
leaves :: GitStore -> IO [BlockHeader]
leaves gs = lockGitStore gs $ \gsd -> leaves' gsd >>= traverse (readHeader gsd)

-- TODO Proper freeing.
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
