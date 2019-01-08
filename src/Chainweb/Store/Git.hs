{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module: Chainweb.Store.Git
-- Copyright: Copyright © 2018 Kadena LLC.
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

  -- * Utilities
  , lockGitStore
  , getSpectrum
  ) where

import qualified Bindings.Libgit2 as Git

import Control.Concurrent.MVar
import Control.Error.Util (hush)
import Control.Exception
    (Exception, bracket, bracketOnError, bracket_, finally, mask, throwIO)
import Control.Monad (void, when)

import Data.Bits (complement, unsafeShiftL, (.&.), (.|.))
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import qualified Data.ByteString.Base58 as B58
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import Data.Word (Word64)

import Foreign.C.String (withCString)
import Foreign.C.Types (CInt, CSize, CUInt)
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)

import Prelude hiding (lookup)

import System.Path (FsPath(..), toAbsoluteFilePath)

-- internal modules

import Chainweb.BlockHash
import Chainweb.BlockHeader

---

data GitStoreConfig = GitStoreConfig {
    _gitStorePath :: FsPath
}

data GitStoreData = GitStoreData {
    _gitStore :: {-# UNPACK #-} !(Ptr Git.C'git_repository)
  , _gitOdb :: {-# UNPACK #-} !(Ptr Git.C'git_odb)
}

-- TODO So, this is to have the instance of `TreeDb`?
newtype GitStore = GitStore (MVar GitStoreData)

newtype GitHash = GitHash ByteString deriving (Eq, Ord, Show)

data TreeEntry = TreeEntry {
    _te_blockHeight :: {-# UNPACK #-} !BlockHeight
  , _te_blockHash :: {-# UNPACK #-} !BlockHashBytes -- TODO Why not `BlockHash`?
  , _te_gitHash :: {-# UNPACK #-} !GitHash
} deriving (Show, Eq, Ord)

data LeafTreeData = LeafTreeData {
    _ltd_blockHeight :: !BlockHeight
  , _ltd_blockHash :: {-# UNPACK #-} !BlockHashBytes
  , _ltd_blockHeaderBlobHash :: !GitHash
  , _ltd_spectrum :: Vector TreeEntry
} deriving (Show)

newtype GitFailure = GitFailure { gitFailureErrorCode :: CInt }
  deriving (Show)

instance Exception GitFailure

newtype GitStoreFailure = GitStoreFailure { gitStoreFailureReason :: Text }
  deriving (Show)

instance Exception GitStoreFailure

lockGitStore :: GitStore -> (GitStoreData -> IO a) -> IO a
lockGitStore (GitStore m) f = withMVar m f

withGitStore :: GitStoreConfig -> (GitStore -> IO a) -> IO a
withGitStore (GitStoreConfig (FsPath root0)) f = Git.withLibGitDo $ do
    root <- toAbsoluteFilePath root0
    bracket (open root) close f
  where
    open :: String -> IO GitStore
    open root =
        mask $ \restore ->
        bracketOnError (openRepo restore root) Git.c'git_repository_free $
        \repo -> do
            odb <- openOdb restore repo
            m <- newMVar (GitStoreData repo odb)
            return $! GitStore m

    close :: GitStore -> IO ()
    close m = lockGitStore m $ \(GitStoreData p o) ->
        Git.c'git_odb_free o `finally` Git.c'git_repository_free p

    --------------------------------------------------------------------------
    openRepo :: (IO CInt -> IO CInt) -> String -> IO (Ptr Git.C'git_repository)
    openRepo restore root =
        tryOpen restore root >>= either (const $ initBare restore root) return

    tryOpen :: (IO CInt -> IO CInt) -> String -> IO (Either CInt (Ptr Git.C'git_repository))
    tryOpen restore root = withCString root $ \proot -> alloca $ \repoptr -> do
        res <- restore (Git.c'git_repository_open_ext repoptr proot openFlags proot)
        if res == 0
          then Right <$> peek repoptr
          else return (Left res)

    --------------------------------------------------------------------------
    initBare :: (IO CInt -> IO CInt) -> String -> IO (Ptr Git.C'git_repository)
    initBare restore root =
      withCString root $ \proot -> alloca $ \repoptr -> do
        throwOnGitError (restore $ Git.c'git_repository_init repoptr proot 1)
        peek repoptr

    --------------------------------------------------------------------------
    -- c'git_repository_odb :: Ptr (Ptr C'git_odb) -> Ptr C'git_repository -> IO CInt
    openOdb :: (IO () -> IO a) -> Ptr Git.C'git_repository -> IO (Ptr Git.C'git_odb)
    openOdb restore repo = alloca $ \podb -> do
        void $ restore $ throwOnGitError $ Git.c'git_repository_odb podb repo
        peek podb

    --------------------------------------------------------------------------
    -- not sure why this flag is not in gitlab2 bindings
    _FLAG_OPEN_BARE :: CUInt
    _FLAG_OPEN_BARE = 4

    openFlags :: CUInt
    openFlags = Git.c'GIT_REPOSITORY_OPEN_NO_SEARCH .|. _FLAG_OPEN_BARE

------------------------------------------------------------------------------
data InsertResult = Inserted | AlreadyExists

insertBlock :: GitStore -> BlockHeader -> IO InsertResult
insertBlock gs bh = lockGitStore gs $ \store -> do
    let hash = getBlockHashBytes $ _blockHash bh
    let height = fromIntegral $ _blockHeight bh
    m <- lookupTreeEntryByHash store hash height
    maybe (go store) (const $ return AlreadyExists) m
  where
    go :: GitStoreData -> IO InsertResult
    go store = createLeafTree store bh >> return Inserted


------------------------------------------------------------------------------
lookupByBlockHash :: GitStore -> BlockHeight -> BlockHash -> IO (Maybe BlockHeader)
lookupByBlockHash gs height bh = lockGitStore gs $ \store -> do
    m <- lookupTreeEntryByHash store (getBlockHashBytes bh) (fromIntegral height)
    traverse (readBlob store) m
  where
    readBlob :: GitStoreData -> TreeEntry -> IO BlockHeader
    readBlob store (TreeEntry _ _ gh) = do
        bs <- getBlob store gh
        either (throwGitStoreFailure . T.pack) return $
            runGetS decodeBlockHeader bs


------------------------------------------------------------------------------
collectGarbage :: GitStore -> IO ()
collectGarbage _ = return $! () -- TODO


------------------------------------------------------------------------------
insertBlockHeaderIntoOdb :: GitStoreData -> BlockHeader -> IO GitHash
insertBlockHeaderIntoOdb (GitStoreData _ odb) bh =
    GitHash <$> B.unsafeUseAsCStringLen serializedBlockHeader write
  where
    !serializedBlockHeader = runPutS $! encodeBlockHeader bh

    write :: (Ptr a, Int) -> IO ByteString
    write (cs, len) = alloca $ \oidPtr -> do
       throwOnGitError $ Git.c'git_odb_write oidPtr odb (castPtr cs)
                                             (fromIntegral len)
                                             Git.c'GIT_OBJ_BLOB
       oidToByteString oidPtr


------------------------------------------------------------------------------
getBlockHashBytes :: BlockHash -> BlockHashBytes
getBlockHashBytes (BlockHash _ bytes) = bytes


------------------------------------------------------------------------------
-- | Insert a tree entry into a @git_treebuilder@.
tbInsert
    :: Ptr Git.C'git_treebuilder
    -> Git.C'git_filemode_t
    -> BlockHeight
    -> BlockHashBytes
    -> GitHash
    -> IO ()
tbInsert tb mode h hs gh =
    withOid gh $ \oid ->
    B.unsafeUseAsCString name $ \cname ->
    throwOnGitError $ Git.c'git_treebuilder_insert nullPtr tb cname oid mode
  where
    name = mkTreeEntryName h hs


------------------------------------------------------------------------------
-- | Given a block header: lookup its parent leaf tree entry, write the block
-- header into the object database, compute a spectrum for the new tree entry,
-- write the @git_tree@ to the repository, tag it under @tags/bh/foo@, and
-- returns the git hash of the new @git_tree@ object.
createLeafTree :: GitStoreData -> BlockHeader -> IO GitHash
createLeafTree store@(GitStoreData repo _) bh = withTreeBuilder $ \treeB -> do
    when (height <= 0) $ throwGitStoreFailure "cannot insert genesis block"
    parentTreeEntry <- lookupTreeEntryByHash store parentHash (height - 1) >>=
                       maybe (throwGitStoreFailure "parent hash not found in DB")
                             return
    let parentTreeGitHash = _te_gitHash parentTreeEntry
    parentTreeData <- readLeafTree store parentTreeGitHash
    treeEntries <- traverse (\h -> lookupTreeEntryByHeight' store parentTreeGitHash h parentTreeData)
                        spectrum
    newHeaderGitHash <- insertBlockHeaderIntoOdb store bh
    mapM_ (addTreeEntry treeB) treeEntries
    addTreeEntry treeB parentTreeEntry
    addSelfEntry treeB height hash newHeaderGitHash
    treeHash <- alloca $ \oid -> do
        throwOnGitError $ Git.c'git_treebuilder_write oid repo treeB
        GitHash <$> oidToByteString oid
    createBlockHeaderTag store bh treeHash

    updateLeafTags store parentTreeEntry (TreeEntry height hash treeHash)

    -- TODO:
    --   - compute total difficulty weight vs the winning block, and atomic-replace
    --     the winning ref (e.g. @tags/BEST@) if the new block is better
    return treeHash

  where
    height :: BlockHeight
    height = _blockHeight bh

    hash :: BlockHashBytes
    hash = getBlockHashBytes $ _blockHash bh

    parentHash :: BlockHashBytes
    parentHash = getBlockHashBytes $ _blockParent bh

    spectrum :: [BlockHeight]
    spectrum = getSpectrum height

    addTreeEntry :: Ptr Git.C'git_treebuilder -> TreeEntry -> IO ()
    addTreeEntry tb (TreeEntry h hs gh) = tbInsert tb Git.c'GIT_FILEMODE_TREE h hs gh

    addSelfEntry :: Ptr Git.C'git_treebuilder -> BlockHeight -> BlockHashBytes -> GitHash -> IO ()
    addSelfEntry tb h hs gh = tbInsert tb Git.c'GIT_FILEMODE_BLOB h hs gh


------------------------------------------------------------------------------
createBlockHeaderTag :: GitStoreData -> BlockHeader -> GitHash -> IO ()
createBlockHeaderTag gs@(GitStoreData repo _) bh leafHash =
    withObject gs leafHash $ \obj ->
    alloca $ \pTagOid ->
    B.unsafeUseAsCString tagName $ \cstr ->
    throwOnGitError (Git.c'git_tag_create_lightweight pTagOid repo cstr obj 1)
  where
    height :: BlockHeight
    height = _blockHeight bh

    hash :: BlockHashBytes
    hash = getBlockHashBytes $ _blockHash bh

    tagName :: ByteString
    tagName = mkTagName height hash


------------------------------------------------------------------------------
parseLeafTreeFileName :: ByteString -> Maybe (BlockHeight, BlockHashBytes)
parseLeafTreeFileName fn = do
    height <- decodeHeight heightStr
    bh <- BlockHashBytes <$> decodeB58 blockHash0
    return (height, bh)
  where
    (heightStr, rest) = B.break (== '.') fn
    blockHash0 = B.drop 1 rest

    decodeHeight :: ByteString -> Maybe BlockHeight
    decodeHeight s = do
      s' <- decodeB58 s
      fromIntegral <$> hush (runGetS decodeBlockHeight s')


------------------------------------------------------------------------------
withOid :: GitHash -> (Ptr Git.C'git_oid -> IO a) -> IO a
withOid (GitHash strOid) f =
    B.unsafeUseAsCStringLen strOid $ \(cstr, clen) -> alloca $ \pOid -> do
        throwOnGitError $ Git.c'git_oid_fromstrn pOid cstr (fromIntegral clen)
        f pOid


------------------------------------------------------------------------------
withObject :: GitStoreData -> GitHash -> (Ptr Git.C'git_object -> IO a) -> IO a
withObject (GitStoreData repo _) hash f =
    withOid hash $ \oid ->
    alloca $ \pobj -> do
        throwOnGitError $ Git.c'git_object_lookup pobj repo oid Git.c'GIT_OBJ_ANY
        peek pobj >>= f


------------------------------------------------------------------------------

-- TODO Does this throw when the `GitHash` doesn't exist in the store?
-- | Fetch the raw byte data of some object in the Git Store.
getBlob :: GitStoreData -> GitHash -> IO ByteString
getBlob (GitStoreData repo _) gh = bracket lookup destroy readBlob
  where
    lookup :: IO (Ptr Git.C'git_blob)
    lookup = mask $ \restore ->
             alloca $ \pBlob ->
             withOid gh $ \oid -> do
        throwOnGitError $ restore $ Git.c'git_blob_lookup pBlob repo oid
        peek pBlob

    destroy :: Ptr Git.C'git_blob -> IO ()
    destroy = Git.c'git_blob_free

    readBlob :: Ptr Git.C'git_blob -> IO ByteString
    readBlob blob = do
        content <- Git.c'git_blob_rawcontent blob
        size <- Git.c'git_blob_rawsize blob
        B.packCStringLen (castPtr content, fromIntegral size)


------------------------------------------------------------------------------
withTreeObject
    :: GitStoreData
    -> GitHash
    -> (Ptr Git.C'git_tree -> IO a)
    -> IO a
withTreeObject (GitStoreData repo _) gitHash f = bracket getTree free f
  where
    getTree :: IO (Ptr Git.C'git_tree)
    getTree = mask $ \restore ->
              alloca $ \ppTree ->
              withOid gitHash $ \oid -> do
                throwOnGitError $ restore $ Git.c'git_tree_lookup ppTree repo oid
                peek ppTree


------------------------------------------------------------------------------
withReference :: GitStoreData
              -> ByteString
              -> (Ptr Git.C'git_reference -> IO a)    -- ^ ptr may be null
              -> IO a
withReference (GitStoreData repo _) path0 f = bracket lookup destroy f
  where
    path :: ByteString
    path = B.append "refs/" path0

    destroy :: Ptr Git.C'git_reference -> IO ()
    destroy p = when (p /= nullPtr) $ Git.c'git_reference_free p

    lookup :: IO (Ptr Git.C'git_reference)
    lookup = mask $ \restore ->
             alloca $ \pRef ->
             B.unsafeUseAsCString path $ \cstr -> do
        code <- restore $ Git.c'git_reference_lookup pRef repo cstr
        if | code == Git.c'GIT_ENOTFOUND -> return nullPtr
           | code /= 0 -> throwGitError code
           | otherwise -> peek pRef


------------------------------------------------------------------------------
withTreeBuilder :: (Ptr Git.C'git_treebuilder -> IO a) -> IO a
withTreeBuilder f =
    alloca $ \pTB -> bracket_ (make pTB)
                              (peek pTB >>= Git.c'git_treebuilder_free)
                              (peek pTB >>= f)
  where
    make :: Ptr (Ptr Git.C'git_treebuilder) -> IO ()
    make p = throwOnGitError (Git.c'git_treebuilder_create p nullPtr)


------------------------------------------------------------------------------
oidToByteString :: Ptr Git.C'git_oid -> IO ByteString
oidToByteString pOid = bracket (Git.c'git_oid_allocfmt pOid) free B.packCString


------------------------------------------------------------------------------
readLeafTree :: GitStoreData -> GitHash -> IO LeafTreeData
readLeafTree store treeGitHash = withTreeObject store treeGitHash readTree
  where
    readTree :: Ptr Git.C'git_tree -> IO LeafTreeData
    readTree pTree = do
      numEntries <- Git.c'git_tree_entrycount pTree
      elist <- mapM (readTreeEntry pTree) [0..(numEntries-1)]
      spectrum <- sortSpectrum elist
      when (V.null spectrum) $ throwGitStoreFailure "impossible: empty tree"
      let lastEntry = V.unsafeLast spectrum
      return $! LeafTreeData (_te_blockHeight lastEntry)
                             (_te_blockHash lastEntry)
                             (_te_gitHash lastEntry)
                             (V.take (V.length spectrum - 1) spectrum)

    readTreeEntry :: Ptr Git.C'git_tree -> CSize -> IO TreeEntry
    readTreeEntry pTree idx =
      bracket (Git.c'git_tree_entry_byindex pTree idx)
              Git.c'git_tree_entry_free
              fromTreeEntryP

    fromTreeEntryP :: Ptr Git.C'git_tree_entry -> IO TreeEntry
    fromTreeEntryP entryP = do
      name <- bracket (Git.c'git_tree_entry_name entryP) free
                      B.packCString
      oid  <- GitHash <$> bracket (Git.c'git_tree_entry_id entryP) free oidToByteString
      (h, bh) <- maybe (throwGitStoreFailure "invalid tree object!") return
                       (parseLeafTreeFileName name)
      return $! TreeEntry h bh oid

    sortSpectrum :: [TreeEntry] -> IO (Vector TreeEntry)
    sortSpectrum l = do
      mv <- V.unsafeThaw (V.fromList l)
      V.sort mv
      V.unsafeFreeze mv


------------------------------------------------------------------------------
updateLeafTags :: GitStoreData -> TreeEntry -> TreeEntry -> IO ()
updateLeafTags store@(GitStoreData repo _) oldLeaf newLeaf = do
    withObject store (_te_gitHash newLeaf) $ \obj ->
        alloca $ \pTagOid ->
        B.unsafeUseAsCString (mkName newLeaf) $ \cstr ->
        throwOnGitError $ Git.c'git_tag_create_lightweight pTagOid repo cstr
                                                           obj 1
    B.unsafeUseAsCString (mkName oldLeaf) $ \cstr ->
        throwOnGitError $ Git.c'git_tag_delete repo cstr

  where
    mkName :: TreeEntry -> ByteString
    mkName (TreeEntry h bh _) = mkLeafTagName h bh


------------------------------------------------------------------------------
mkTreeEntryName :: BlockHeight -> BlockHashBytes -> ByteString
mkTreeEntryName height hash = B.concat [ encHeight, ".", encBH ]
  where
    encBH = bsToB58 $! runPutS (encodeBlockHashBytes hash)
    encHeight = bsToB58 $! runPutS (encodeBlockHeight height)

mkTagName :: BlockHeight -> BlockHashBytes -> ByteString
mkTagName height hash = B.append "bh/" (mkTreeEntryName height hash)

mkLeafTagName :: BlockHeight -> BlockHashBytes -> ByteString
mkLeafTagName height hash = B.append "leaf/" (mkTreeEntryName height hash)

mkTagRef :: BlockHeight -> BlockHashBytes -> ByteString
mkTagRef height hash = B.append "tags/" (mkTagName height hash)


------------------------------------------------------------------------------
lookupRefTarget
    :: GitStoreData
    -- TODO Can this be a more rigorous path type?
    -> ByteString        -- ^ ref path, e.g. tags/foo
    -> IO (Maybe GitHash)
lookupRefTarget (GitStoreData repo _) path0 =
    B.unsafeUseAsCString path $ \cpath ->
    alloca $ \pOid -> do
        code <- Git.c'git_reference_name_to_id pOid repo cpath
        if code /= 0
          then return Nothing
          else Just . GitHash <$> oidToByteString pOid

  where
    path :: ByteString
    path = B.append "refs/" path0


------------------------------------------------------------------------------
lookupTreeEntryByHash
    :: GitStoreData
    -> BlockHashBytes
    -> BlockHeight
    -> IO (Maybe TreeEntry)
lookupTreeEntryByHash gs bh height = do
    when (height == 0) $ throwGitStoreFailure "TODO: handle genesis block"
    lookupRefTarget gs tagRef >>=
      traverse (\gitHash -> return $! TreeEntry height bh gitHash)
  where
    tagRef :: ByteString
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
lookupTreeEntryByHeight' gs leafTreeHash height (LeafTreeData leafHeight leafBH _ spectrum) = do
    when (height < 0) $ throwGitStoreFailure "height must be non-negative"
    if | height == leafHeight -> return $! TreeEntry height leafBH leafTreeHash
       | height == 0 -> genesisBlockHashes gs
       | V.null spec' -> throwGitStoreFailure "lookup failure"
       | otherwise -> search
  where
    spec' :: Vector TreeEntry
    spec' = V.filter (\t -> _te_blockHeight t >= height) spectrum

    search :: IO TreeEntry
    search = do
        let first = V.unsafeHead spec'
        let gh = _te_gitHash first
        if _te_blockHeight first == height
          then return first
          else lookupTreeEntryByHeight gs gh height


------------------------------------------------------------------------------
getSpectrum :: BlockHeight -> [BlockHeight]
getSpectrum (BlockHeight d0) = map BlockHeight . dedup $ startSpec ++ rlgs ++ recents
  where
    numRecents = 4
    d = max 0 (d0 - numRecents)
    recents = [d..(max 0 (d0-2))]       -- don't include d0 or its parent

    pow2s = [ 1 `unsafeShiftL` x | x <- [5..63] ]

    (startSpec, lastSpec) = fs id 0 pow2s
    diff = d - lastSpec

    -- reverse log spectrum should be quantized on the lower bits
    quantize :: Word64 -> Word64
    quantize !x = let !out = (d - x) .&. complement (x-1) in out

    lgs = map quantize $ takeWhile (< diff) pow2s
    rlgs = reverse lgs

    fs :: ([Word64] -> a) -> Word64 -> [Word64] -> (a, Word64)
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
bsToB58 :: ByteString -> ByteString
bsToB58 = B58.encodeBase58 B58.bitcoinAlphabet

decodeB58 :: ByteString -> Maybe ByteString
decodeB58 = B58.decodeBase58 B58.bitcoinAlphabet

intToB58 :: Integer -> ByteString
intToB58 = B58.encodeBase58I B58.bitcoinAlphabet


------------------------------------------------------------------------------
throwOnGitError :: IO CInt -> IO ()
throwOnGitError m = do
  code <- m
  when (code /= 0) $ throwGitError code

throwGitError :: CInt -> IO a
throwGitError = throwIO . GitFailure


------------------------------------------------------------------------------
throwGitStoreFailure :: Text -> IO a
throwGitStoreFailure = throwIO . GitStoreFailure


------------------------------------------------------------------------------
-- TODO: needs more arguments here unless we put genesis block into
-- GitStoreData
insertGenesisBlock :: GitStoreData -> IO ()
insertGenesisBlock _ = undefined

genesisBlockGitHash :: GitStoreData -> IO GitHash
genesisBlockGitHash _ = undefined

genesisBlockHashes :: GitStoreData -> IO TreeEntry
genesisBlockHashes = undefined
