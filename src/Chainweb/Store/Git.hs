{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Chainweb.Store.Git
  ( GitStoreConfig(..)
  , GitStore
  , GitFailure(..)
  -- ** opening a git chainweb store
  , withGitStore

  -- ** inserting blocks into the store
  , InsertResult(..)
  , insertBlock

  -- ** lookups
  , lookupByBlockHash

  -- ** utilities
  , lockGitStore
  , getSpectrum
  ) where

import qualified Bindings.Libgit2 as Git
import Control.Concurrent.MVar
import Control.Exception
    (Exception, bracket, bracketOnError, finally, mask, throwIO)
import Control.Monad (void, when)
import Data.Bits (complement, unsafeShiftL, (.&.), (.|.))
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import qualified Data.ByteString.Base58 as B58
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Unsafe as S
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import Prelude
    (Bool(..), Either(..), Eq(..), IO, Integer, Maybe(..), Monad(..), Num(..),
    Ord(..), Show(..), all, const, either, fromIntegral, id, map, mapM, mapM_,
    maybe, otherwise, reverse, takeWhile, undefined, ($), ($!), (&&), (++),
    (.), (<$>))
import System.Path (FsPath(..), toAbsoluteFilePath)

------------------------------------------------------------------------------
import Chainweb.BlockHash
import Chainweb.BlockHeader
------------------------------------------------------------------------------


------------------------------------------------------------------------------
data GitStoreConfig = GitStoreConfig {
    _gitStorePath :: FsPath
}


------------------------------------------------------------------------------
data GitStoreData = GitStoreData {
    _gitStore :: {-# UNPACK #-} !(Ptr Git.C'git_repository)
  , _gitOdb :: {-# UNPACK #-} !(Ptr Git.C'git_odb)
}

newtype GitStore = GitStore (MVar GitStoreData)


------------------------------------------------------------------------------
-- TODO: newtype?
type GitHash = ByteString


------------------------------------------------------------------------------
data TreeEntry = TreeEntry {
    _te_blockHeight :: {-# UNPACK #-} !Int64
  , _te_blockHash :: {-# UNPACK #-} !BlockHashBytes
  , _te_gitHash :: {-# UNPACK #-} !GitHash
} deriving (Show, Eq, Ord)

data LeafTreeData = LeafTreeData {
    _ltd_blockHeight :: !Int64
  , _ltd_blockHash :: {-# UNPACK #-} !BlockHashBytes
  , _ltd_blockHeaderBlobHash :: !GitHash
  , _ltd_spectrum :: Vector TreeEntry
} deriving (Show)


------------------------------------------------------------------------------
data GitFailure = GitFailure { gitFailureErrorCode :: !CInt }
  deriving (Show)
instance Exception GitFailure

data GitStoreFailure = GitStoreFailure { gitStoreFailureReason :: !Text }
  deriving (Show)
instance Exception GitStoreFailure


------------------------------------------------------------------------------
lockGitStore :: GitStore -> (GitStoreData -> IO a) -> IO a
lockGitStore (GitStore m) f = withMVar m f


------------------------------------------------------------------------------
withGitStore :: GitStoreConfig -> (GitStore -> IO a) -> IO a
withGitStore (GitStoreConfig (FsPath root0)) f = Git.withLibGitDo $ do
    root <- toAbsoluteFilePath root0
    bracket (open root) close f

  where
    --------------------------------------------------------------------------
    open root =
        mask $ \restore ->
        bracketOnError (openRepo restore root) Git.c'git_repository_free $
        \repo -> do
            odb <- openOdb restore repo
            m <- newMVar (GitStoreData repo odb)
            return $! GitStore m

    close m = lockGitStore m $ \(GitStoreData p o) ->
                               Git.c'git_odb_free o `finally`
                               Git.c'git_repository_free p

    --------------------------------------------------------------------------
    openRepo restore root = tryOpen restore root >>=
                            either (const $ initBare restore root) return

    tryOpen restore root = withCString root $
                           \proot -> alloca $
                           \repoptr -> do
      res <- restore (Git.c'git_repository_open_ext repoptr proot openFlags proot)
      if res == 0
        then Right <$> peek repoptr
        else return $! Left res

    --------------------------------------------------------------------------
    initBare restore root =
      withCString root $ \proot -> alloca $ \repoptr -> do
        throwOnGitError (restore $ Git.c'git_repository_init repoptr proot 1)
        peek repoptr

    --------------------------------------------------------------------------
    -- c'git_repository_odb :: Ptr (Ptr C'git_odb) -> Ptr C'git_repository -> IO CInt
    openOdb restore repo = alloca $ \podb -> do
        void $ restore $ throwOnGitError $ Git.c'git_repository_odb podb repo
        peek podb

    --------------------------------------------------------------------------
    -- not sure why this flag is not in gitlab2 bindings
    _FLAG_OPEN_BARE = 4
    openFlags = Git.c'GIT_REPOSITORY_OPEN_NO_SEARCH .|.
                _FLAG_OPEN_BARE

------------------------------------------------------------------------------
data InsertResult = Inserted | AlreadyExists

insertBlock :: GitStore -> BlockHeader -> IO InsertResult
insertBlock gs bh = lockGitStore gs $ \store -> do
    let hash = getBlockHashBytes $ _blockHash bh
    let height = fromIntegral $ _blockHeight bh
    m <- lookupTreeEntryByHash store hash height
    maybe (go store) (const $ return AlreadyExists) m
  where
    go store = createLeafTree store bh >> return Inserted


------------------------------------------------------------------------------
lookupByBlockHash :: GitStore -> BlockHeight -> BlockHash -> IO (Maybe BlockHeader)
lookupByBlockHash gs height bh = lockGitStore gs $ \store -> do
    m <- lookupTreeEntryByHash store (getBlockHashBytes bh) (fromIntegral height)
    mapM (readBlob store) m
  where
    readBlob store (TreeEntry _ _ gh) = do
        bs <- getBlob store gh
        either (throwGitStoreFailure . T.pack) return $
            runGetS decodeBlockHeader bs


------------------------------------------------------------------------------
collectGarbage :: GitStore -> IO ()
collectGarbage _ = return $! () -- TODO


------------------------------------------------------------------------------
insertBlockHeaderIntoOdb :: GitStoreData -> BlockHeader -> IO GitHash
insertBlockHeaderIntoOdb (GitStoreData _ odb) bh = do
    S.unsafeUseAsCStringLen serializedBlockHeader write
  where
    !serializedBlockHeader = runPutS $! encodeBlockHeader bh
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
tbInsert :: Ptr Git.C'git_treebuilder
         -> Git.C'git_filemode_t
         -> Int64
         -> BlockHashBytes
         -> ByteString
         -> IO ()
tbInsert tb mode h hs gh =
    withOid gh $ \oid ->
    S.unsafeUseAsCString name $ \cname ->
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
    treeEntries <- mapM (\h -> lookupTreeEntryByHeight' store parentTreeGitHash h parentTreeData)
                        spectrum
    newHeaderGitHash <- insertBlockHeaderIntoOdb store bh
    mapM_ (addTreeEntry treeB) treeEntries
    addTreeEntry treeB parentTreeEntry
    addSelfEntry treeB height hash newHeaderGitHash
    treeHash <- alloca $ \oid -> do
        throwOnGitError $ Git.c'git_treebuilder_write oid repo treeB
        oidToByteString oid
    createBlockHeaderTag store bh treeHash

    updateLeafTags store parentTreeEntry (TreeEntry height hash treeHash)

    -- TODO:
    --   - compute total difficulty weight vs the winning block, and atomic-replace
    --     the winning ref (e.g. @tags/BEST@) if the new block is better
    return treeHash

  where
    height = (fromIntegral $ _blockHeight bh) :: Int64
    hash = getBlockHashBytes $ _blockHash bh
    parentHash = getBlockHashBytes $ _blockParent bh

    spectrum = getSpectrum height

    addTreeEntry tb (TreeEntry h hs gh) = tbInsert tb Git.c'GIT_FILEMODE_TREE
                                                   h hs gh
    addSelfEntry tb h hs gh = tbInsert tb Git.c'GIT_FILEMODE_BLOB h hs gh


------------------------------------------------------------------------------
createBlockHeaderTag :: GitStoreData -> BlockHeader -> GitHash -> IO ()
createBlockHeaderTag gs@(GitStoreData repo _) bh leafHash =
    withObject gs leafHash $ \obj ->
    alloca $ \pTagOid ->
    S.unsafeUseAsCString tagName $ \cstr ->
    throwOnGitError (Git.c'git_tag_create_lightweight pTagOid repo cstr obj 1)
  where
    height = fromIntegral $ _blockHeight bh
    hash = getBlockHashBytes $ _blockHash bh
    tagName = mkTagName height hash


------------------------------------------------------------------------------
parseLeafTreeFileName :: ByteString -> Maybe (Int64, BlockHashBytes)
parseLeafTreeFileName fn = do
    height <- decodeHeight heightStr
    bh <- BlockHashBytes <$> decodeB58 blockHash0
    return $! (height, bh)
  where
    (heightStr, rest) = S.break (== '.') fn
    blockHash0 = S.drop 1 rest

    eitherToMaybe = either (const Nothing) Just

    decodeHeight s = do
      s' <- decodeB58 s
      fromIntegral <$> eitherToMaybe (runGetS decodeBlockHeight s')



------------------------------------------------------------------------------
withOid :: ByteString -> (Ptr Git.C'git_oid -> IO a) -> IO a
withOid strOid f = S.unsafeUseAsCStringLen strOid $ \(cstr, clen) ->
                   alloca $ \pOid -> do
    throwOnGitError $ Git.c'git_oid_fromstrn pOid cstr (fromIntegral clen)
    f pOid


------------------------------------------------------------------------------
withObject :: GitStoreData -> ByteString -> (Ptr Git.C'git_object -> IO a) -> IO a
withObject (GitStoreData repo _) hash f =
    withOid hash $ \oid ->
    alloca $ \pobj -> do
        throwOnGitError $ Git.c'git_object_lookup pobj repo oid Git.c'GIT_OBJ_ANY
        peek pobj >>= f


------------------------------------------------------------------------------
getBlob :: GitStoreData -> GitHash -> IO ByteString
getBlob (GitStoreData repo _) gh = bracket lookup destroy readBlob
  where
    lookup = mask $ \restore ->
             alloca $ \pBlob ->
             withOid gh $ \oid -> do
        throwOnGitError $ restore $ Git.c'git_blob_lookup pBlob repo oid
        peek pBlob
    destroy = Git.c'git_blob_free
    readBlob blob = do
        content <- Git.c'git_blob_rawcontent blob
        size <- Git.c'git_blob_rawsize blob
        S.packCStringLen (castPtr content, fromIntegral size)


------------------------------------------------------------------------------
withTreeObject :: GitStoreData
               -> ByteString
               -> (Ptr Git.C'git_tree -> IO a)
               -> IO a
withTreeObject (GitStoreData repo _) gitHash f =
    bracket getTree free f
  where
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
withReference (GitStoreData repo _) path0 f =
    bracket lookup destroy f
  where
    path = S.append "refs/" path0

    destroy p = when (p /= nullPtr) $ Git.c'git_reference_free p

    lookup = mask $ \restore ->
             alloca $ \pRef ->
             S.unsafeUseAsCString path $ \cstr -> do
        code <- restore $ Git.c'git_reference_lookup pRef repo cstr
        if code == Git.c'GIT_ENOTFOUND
           then return nullPtr
           else if code /= 0
                   then throwGitError code
                   else peek pRef


------------------------------------------------------------------------------
withTreeBuilder
  :: (Ptr Git.C'git_treebuilder -> IO a) -> IO a
withTreeBuilder f =
    alloca $ \pTB -> bracket (make pTB)
                             (const (peek pTB >>= Git.c'git_treebuilder_free))
                             (const (peek pTB >>= f))
  where
    make p = throwOnGitError (Git.c'git_treebuilder_create p nullPtr)


------------------------------------------------------------------------------
oidToByteString :: Ptr Git.C'git_oid -> IO ByteString
oidToByteString pOid = bracket (Git.c'git_oid_allocfmt pOid) free S.packCString


------------------------------------------------------------------------------
readLeafTree :: GitStoreData -> GitHash -> IO LeafTreeData
readLeafTree store treeGitHash = withTreeObject store treeGitHash readTree
  where
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

    readTreeEntry pTree idx =
      bracket (Git.c'git_tree_entry_byindex pTree idx)
              Git.c'git_tree_entry_free
              fromTreeEntryP

    fromTreeEntryP entryP = do
      name <- bracket (Git.c'git_tree_entry_name entryP) free
                      S.packCString
      oid  <- bracket (Git.c'git_tree_entry_id entryP) free oidToByteString
      (h, bh) <- maybe (throwGitStoreFailure "invalid tree object!") return
                       (parseLeafTreeFileName name)
      return $! TreeEntry h bh oid

    sortSpectrum l = do
      mv <- V.unsafeThaw (V.fromList l)
      V.sort mv
      V.unsafeFreeze mv


------------------------------------------------------------------------------
updateLeafTags :: GitStoreData -> TreeEntry -> TreeEntry -> IO ()
updateLeafTags store@(GitStoreData repo _) oldLeaf newLeaf = do
    withObject store (_te_gitHash newLeaf) $ \obj ->
        alloca $ \pTagOid ->
        S.unsafeUseAsCString (mkName newLeaf) $ \cstr ->
        throwOnGitError $ Git.c'git_tag_create_lightweight pTagOid repo cstr
                                                           obj 1
    S.unsafeUseAsCString (mkName oldLeaf) $ \cstr ->
        throwOnGitError $ Git.c'git_tag_delete repo cstr

  where
    mkName (TreeEntry h bh _) = mkLeafTagName h bh


------------------------------------------------------------------------------
mkTreeEntryName :: Int64 -> BlockHashBytes -> ByteString
mkTreeEntryName height hash = S.concat [ encHeight, ".", encBH ]
  where
    encBH = bsToB58 $! runPutS (encodeBlockHashBytes hash)
    encHeight = bsToB58 $! runPutS (encodeBlockHeight $ fromIntegral height)

mkTagName :: Int64 -> BlockHashBytes -> ByteString
mkTagName height hash = S.append "bh/" (mkTreeEntryName height hash)

mkLeafTagName :: Int64 -> BlockHashBytes -> ByteString
mkLeafTagName height hash = S.append "leaf/" (mkTreeEntryName height hash)

mkTagRef :: Int64 -> BlockHashBytes -> ByteString
mkTagRef height hash = S.append "tags/" (mkTagName height hash)


------------------------------------------------------------------------------
lookupRefTarget :: GitStoreData
                -> ByteString        -- ^ ref path, e.g. tags/foo
                -> IO (Maybe GitHash)
lookupRefTarget (GitStoreData repo _) path0 =
    S.unsafeUseAsCString path $ \cpath ->
    alloca $ \pOid -> do
        code <- Git.c'git_reference_name_to_id pOid repo cpath
        if code /= 0
          then return Nothing
          else Just <$> oidToByteString pOid

  where
    path = S.append "refs/" path0


------------------------------------------------------------------------------
lookupTreeEntryByHash :: GitStoreData
                      -> BlockHashBytes
                      -> Int64
                      -> IO (Maybe TreeEntry)
lookupTreeEntryByHash gs bh height = do
    when (height == 0) $ throwGitStoreFailure "TODO: handle genesis block"
    lookupRefTarget gs tagRef >>=
      mapM (\gitHash -> return $! TreeEntry height bh gitHash)

  where
    tagRef = mkTagRef height bh


------------------------------------------------------------------------------
lookupTreeEntryByHeight :: GitStoreData
                        -> GitHash         -- ^ starting from this leaf tree
                        -> Int64           -- ^ desired blockheight
                        -> IO TreeEntry
lookupTreeEntryByHeight gs leafTreeHash height =
    readLeafTree gs leafTreeHash >>=
    lookupTreeEntryByHeight' gs leafTreeHash height


------------------------------------------------------------------------------
lookupTreeEntryByHeight' :: GitStoreData
                        -> GitHash
                        -> Int64           -- ^ desired blockheight
                        -> LeafTreeData
                        -> IO TreeEntry
lookupTreeEntryByHeight' gs leafTreeHash height (LeafTreeData leafHeight leafBH _
                                           spectrum) = do
    when (height < 0) $ throwGitStoreFailure "height must be non-negative"
    if height == leafHeight
      then return $! TreeEntry height leafBH leafTreeHash
      else if height == 0
             then genesisBlockHashes gs
             else if V.null spec'
                    then throwGitStoreFailure "lookup failure"
                    else search
  where
    spec' = V.filter (\t -> _te_blockHeight t >= height) spectrum
    search = do
        let first = V.unsafeHead spec'
        let gh = _te_gitHash first
        if _te_blockHeight first == height
          then return first
          else lookupTreeEntryByHeight gs gh height


------------------------------------------------------------------------------
getSpectrum :: Int64 -> [Int64]
getSpectrum d0 = dedup $ startSpec ++ rlgs ++ recents
  where
    numRecents = 4
    d = max 0 (d0 - numRecents)
    recents = [d..(max 0 (d0-2))]       -- don't include d0 or its parent

    pow2s = [ 1 `unsafeShiftL` x | x <- [5..63] ]

    (startSpec, lastSpec) = fs id 0 pow2s
    diff = d - lastSpec

    -- reverse log spectrum should be quantized on the lower bits
    quantize !x = let !out = (d - x) .&. complement (x-1) in out

    lgs = map quantize $ takeWhile (< diff) $ pow2s
    rlgs = reverse lgs

    fs !dl !lst (x:zs) | x < d     = fs (dl . (x:)) x zs
                       | otherwise = (dl [], lst)
    fs !dl !lst [] = (dl [], lst)


------------------------------------------------------------------------------
_isSorted :: Ord a => [a] -> Bool
_isSorted [] = True
_isSorted (_:[]) = True
_isSorted (x:z@(y:_)) = x < y && _isSorted z


_prop_spectra_sorted :: Bool
_prop_spectra_sorted = all _isSorted $ map getSpectrum [1,10000000 :: Int64]


------------------------------------------------------------------------------
dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup o@(_:[]) = o
dedup (x:r@(y:_)) | x == y = (dedup r)
                  | otherwise = x:(dedup r)


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

