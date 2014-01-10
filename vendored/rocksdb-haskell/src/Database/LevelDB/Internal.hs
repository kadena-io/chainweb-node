{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Database.LevelDB.Internal
-- Copyright   : (c) 2012-2013 The leveldb-haskell Authors
-- License     : BSD3
-- Maintainer  : kim.altintop@gmail.com
-- Stability   : experimental
-- Portability : non-portable
--

module Database.LevelDB.Internal
    ( -- * Types
      DB (..)
    , Comparator'
    , FilterPolicy'
    , Options' (..)

    -- * "Smart" constructors and deconstructors
    , freeCReadOpts
    , freeComparator
    , freeFilterPolicy
    , freeOpts
    , mkCReadOpts
    , mkComparator
    , mkCompareFun
    , mkCreateFilterFun
    , mkFilterPolicy
    , mkKeyMayMatchFun
    , mkOpts

    -- * combinators
    , withCWriteOpts
    , withCReadOpts

    -- * Utilities
    , throwIfErr
    , cSizeToInt
    , intToCSize
    , intToCInt
    , cIntToInt
    , boolToNum
    )
where

import           Control.Applicative    ((<$>))
import           Control.Exception      (bracket, onException, throwIO)
import           Control.Monad          (when)
import           Data.ByteString        (ByteString)
import           Foreign
import           Foreign.C.String       (peekCString, withCString)
import           Foreign.C.Types        (CInt, CSize)

import           Database.LevelDB.C
import           Database.LevelDB.Types

import qualified Data.ByteString        as BS


-- | Database handle
data DB = DB LevelDBPtr Options'

instance Eq DB where
    (DB pt1 _) == (DB pt2 _) = pt1 == pt2

-- | Internal representation of a 'Comparator'
data Comparator' = Comparator' (FunPtr CompareFun)
                               (FunPtr Destructor)
                               (FunPtr NameFun)
                               ComparatorPtr

-- | Internal representation of a 'FilterPolicy'
data FilterPolicy' = FilterPolicy' (FunPtr CreateFilterFun)
                                   (FunPtr KeyMayMatchFun)
                                   (FunPtr Destructor)
                                   (FunPtr NameFun)
                                   FilterPolicyPtr

-- | Internal representation of the 'Options'
data Options' = Options'
    { _optsPtr  :: !OptionsPtr
    , _cachePtr :: !(Maybe CachePtr)
    , _comp     :: !(Maybe Comparator')
    , _fpPtr    :: !(Maybe (Either FilterPolicyPtr FilterPolicy'))
    }


mkOpts :: Options -> IO Options'
mkOpts Options{..} = do
    opts_ptr <- c_leveldb_options_create

    c_leveldb_options_set_block_restart_interval opts_ptr
        $ intToCInt blockRestartInterval
    c_leveldb_options_set_block_size opts_ptr
        $ intToCSize blockSize
    c_leveldb_options_set_compression opts_ptr
        $ ccompression compression
    c_leveldb_options_set_create_if_missing opts_ptr
        $ boolToNum createIfMissing
    c_leveldb_options_set_error_if_exists opts_ptr
        $ boolToNum errorIfExists
    c_leveldb_options_set_max_open_files opts_ptr
        $ intToCInt maxOpenFiles
    c_leveldb_options_set_paranoid_checks opts_ptr
        $ boolToNum paranoidChecks
    c_leveldb_options_set_write_buffer_size opts_ptr
        $ intToCSize writeBufferSize

    cache <- maybeSetCache opts_ptr cacheSize
    cmp   <- maybeSetCmp opts_ptr comparator
    fp    <- maybeSetFilterPolicy opts_ptr filterPolicy

    return (Options' opts_ptr cache cmp fp)

  where
    ccompression NoCompression = noCompression
    ccompression Snappy        = snappyCompression

    maybeSetCache :: OptionsPtr -> Int -> IO (Maybe CachePtr)
    maybeSetCache opts_ptr size =
        if size <= 0
            then return Nothing
            else do
                cache_ptr <- c_leveldb_cache_create_lru $ intToCSize size
                c_leveldb_options_set_cache opts_ptr cache_ptr
                return . Just $ cache_ptr

    maybeSetCmp :: OptionsPtr -> Maybe Comparator -> IO (Maybe Comparator')
    maybeSetCmp opts_ptr (Just mcmp) = Just <$> setcmp opts_ptr mcmp
    maybeSetCmp _ Nothing = return Nothing

    setcmp :: OptionsPtr -> Comparator -> IO Comparator'
    setcmp opts_ptr (Comparator cmp) = do
        cmp'@(Comparator' _ _ _ cmp_ptr) <- mkComparator "user-defined" cmp
        c_leveldb_options_set_comparator opts_ptr cmp_ptr
        return cmp'

    maybeSetFilterPolicy :: OptionsPtr
                         -> Maybe (Either BloomFilter FilterPolicy)
                         -> IO (Maybe (Either FilterPolicyPtr FilterPolicy'))
    maybeSetFilterPolicy _ Nothing =
        return Nothing
    maybeSetFilterPolicy opts_ptr (Just (Left (BloomFilter bloom_ptr))) = do
        c_leveldb_options_set_filter_policy opts_ptr bloom_ptr
        return Nothing -- bloom filter is freed automatically
    maybeSetFilterPolicy opts_ptr (Just (Right fp)) = do
        fp'@(FilterPolicy' _ _ _ _ fp_ptr) <- mkFilterPolicy fp
        c_leveldb_options_set_filter_policy opts_ptr fp_ptr
        return . Just . Right $ fp'

freeOpts :: Options' -> IO ()
freeOpts (Options' opts_ptr mcache_ptr mcmp_ptr mfp) = do
    c_leveldb_options_destroy opts_ptr
    maybe (return ()) c_leveldb_cache_destroy mcache_ptr
    maybe (return ()) freeComparator mcmp_ptr
    maybe (return ())
          (either c_leveldb_filterpolicy_destroy freeFilterPolicy)
          mfp

    return ()

withCWriteOpts :: WriteOptions -> (WriteOptionsPtr -> IO a) -> IO a
withCWriteOpts WriteOptions{..} = bracket mkCWriteOpts freeCWriteOpts
  where
    mkCWriteOpts = do
        opts_ptr <- c_leveldb_writeoptions_create
        onException
            (c_leveldb_writeoptions_set_sync opts_ptr $ boolToNum sync)
            (c_leveldb_writeoptions_destroy opts_ptr)
        return opts_ptr

    freeCWriteOpts = c_leveldb_writeoptions_destroy

mkCompareFun :: (ByteString -> ByteString -> Ordering) -> CompareFun
mkCompareFun cmp = cmp'
  where
    cmp' _ a alen b blen = do
        a' <- BS.packCStringLen (a, fromInteger . toInteger $ alen)
        b' <- BS.packCStringLen (b, fromInteger . toInteger $ blen)
        return $ case cmp a' b' of
                     EQ ->  0
                     GT ->  1
                     LT -> -1

mkComparator :: String -> (ByteString -> ByteString -> Ordering) -> IO Comparator'
mkComparator name f =
    withCString name $ \cs -> do
        ccmpfun <- mkCmp . mkCompareFun $ f
        cdest   <- mkDest $ const ()
        cname   <- mkName $ const cs
        ccmp    <- c_leveldb_comparator_create nullPtr cdest ccmpfun cname
        return $ Comparator' ccmpfun cdest cname ccmp


freeComparator :: Comparator' -> IO ()
freeComparator (Comparator' ccmpfun cdest cname ccmp) = do
    c_leveldb_comparator_destroy ccmp
    freeHaskellFunPtr ccmpfun
    freeHaskellFunPtr cdest
    freeHaskellFunPtr cname

mkCreateFilterFun :: ([ByteString] -> ByteString) -> CreateFilterFun
mkCreateFilterFun f = f'
  where
    f' _ ks ks_lens n_ks flen = do
        let n_ks' = fromInteger . toInteger $ n_ks
        ks'      <- peekArray n_ks' ks
        ks_lens' <- peekArray n_ks' ks_lens
        keys     <- mapM bstr (zip ks' ks_lens')
        let res = f keys
        poke flen (fromIntegral . BS.length $ res)
        BS.useAsCString res $ \cstr -> return cstr

    bstr (x,len) = BS.packCStringLen (x, fromInteger . toInteger $ len)

mkKeyMayMatchFun :: (ByteString -> ByteString -> Bool) -> KeyMayMatchFun
mkKeyMayMatchFun g = g'
  where
    g' _ k klen f flen = do
        k' <- BS.packCStringLen (k, fromInteger . toInteger $ klen)
        f' <- BS.packCStringLen (f, fromInteger . toInteger $ flen)
        return . boolToNum $ g k' f'


mkFilterPolicy :: FilterPolicy -> IO FilterPolicy'
mkFilterPolicy FilterPolicy{..} =
    withCString fpName $ \cs -> do
        cname  <- mkName $ const cs
        cdest  <- mkDest $ const ()
        ccffun <- mkCF . mkCreateFilterFun $ createFilter
        ckmfun <- mkKMM . mkKeyMayMatchFun $ keyMayMatch
        cfp    <- c_leveldb_filterpolicy_create nullPtr cdest ccffun ckmfun cname

        return $ FilterPolicy' ccffun ckmfun cdest cname cfp

freeFilterPolicy :: FilterPolicy' -> IO ()
freeFilterPolicy (FilterPolicy' ccffun ckmfun cdest cname cfp) = do
    c_leveldb_filterpolicy_destroy cfp
    freeHaskellFunPtr ccffun
    freeHaskellFunPtr ckmfun
    freeHaskellFunPtr cdest
    freeHaskellFunPtr cname

mkCReadOpts :: ReadOptions -> IO ReadOptionsPtr
mkCReadOpts ReadOptions{..} = do
    opts_ptr <- c_leveldb_readoptions_create
    flip onException (c_leveldb_readoptions_destroy opts_ptr) $ do
        c_leveldb_readoptions_set_verify_checksums opts_ptr $ boolToNum verifyCheckSums
        c_leveldb_readoptions_set_fill_cache opts_ptr $ boolToNum fillCache

        case useSnapshot of
            Just (Snapshot snap_ptr) -> c_leveldb_readoptions_set_snapshot opts_ptr snap_ptr
            Nothing -> return ()

    return opts_ptr

freeCReadOpts :: ReadOptionsPtr -> IO ()
freeCReadOpts = c_leveldb_readoptions_destroy

withCReadOpts :: ReadOptions -> (ReadOptionsPtr -> IO a) -> IO a
withCReadOpts opts = bracket (mkCReadOpts opts) freeCReadOpts

throwIfErr :: String -> (ErrPtr -> IO a) -> IO a
throwIfErr s f = alloca $ \err_ptr -> do
    poke err_ptr nullPtr
    res  <- f err_ptr
    erra <- peek err_ptr
    when (erra /= nullPtr) $ do
        err <- peekCString erra
        throwIO $ userError $ s ++ ": " ++ err
    return res

cSizeToInt :: CSize -> Int
cSizeToInt = fromIntegral
{-# INLINE cSizeToInt #-}

intToCSize :: Int -> CSize
intToCSize = fromIntegral
{-# INLINE intToCSize #-}

intToCInt :: Int -> CInt
intToCInt = fromIntegral
{-# INLINE intToCInt #-}

cIntToInt :: CInt -> Int
cIntToInt = fromIntegral
{-# INLINE cIntToInt #-}

boolToNum :: Num b => Bool -> b
boolToNum True  = fromIntegral (1 :: Int)
boolToNum False = fromIntegral (0 :: Int)
{-# INLINE boolToNum #-}
