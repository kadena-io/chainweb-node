{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Database.RocksDB.Internal
-- Copyright   : (c) 2012-2013 The leveldb-haskell Authors
--               (c) 2014 The rocksdb-haskell Authors
-- License     : BSD3
-- Maintainer  : mail@agrafix.net
-- Stability   : experimental
-- Portability : non-portable
--

module Database.RocksDB.Internal
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

import           Database.RocksDB.C
import           Database.RocksDB.Types

import qualified Data.ByteString        as BS


-- | Database handle
data DB = DB RocksDBPtr Options'

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
    }


mkOpts :: Options -> IO Options'
mkOpts Options{..} = do
    opts_ptr <- c_rocksdb_options_create

    c_rocksdb_options_set_compression opts_ptr
        $ ccompression compression
    c_rocksdb_options_set_create_if_missing opts_ptr
        $ boolToNum createIfMissing
    c_rocksdb_options_set_error_if_exists opts_ptr
        $ boolToNum errorIfExists
    c_rocksdb_options_set_max_open_files opts_ptr
        $ intToCInt maxOpenFiles
    c_rocksdb_options_set_paranoid_checks opts_ptr
        $ boolToNum paranoidChecks
    c_rocksdb_options_set_write_buffer_size opts_ptr
        $ intToCSize writeBufferSize

    cmp   <- maybeSetCmp opts_ptr comparator

    return (Options' opts_ptr Nothing cmp)

  where
    ccompression NoCompression =
        noCompression
    ccompression SnappyCompression =
        snappyCompression
    ccompression ZlibCompression =
        zlibCompression

    maybeSetCmp :: OptionsPtr -> Maybe Comparator -> IO (Maybe Comparator')
    maybeSetCmp opts_ptr (Just mcmp) = Just <$> setcmp opts_ptr mcmp
    maybeSetCmp _ Nothing            = return Nothing

    setcmp :: OptionsPtr -> Comparator -> IO Comparator'
    setcmp opts_ptr (Comparator cmp) = do
        cmp'@(Comparator' _ _ _ cmp_ptr) <- mkComparator "user-defined" cmp
        c_rocksdb_options_set_comparator opts_ptr cmp_ptr
        return cmp'

freeOpts :: Options' -> IO ()
freeOpts (Options' opts_ptr mcache_ptr mcmp_ptr ) = do
    c_rocksdb_options_destroy opts_ptr
    maybe (return ()) c_rocksdb_cache_destroy mcache_ptr
    maybe (return ()) freeComparator mcmp_ptr

withCWriteOpts :: WriteOptions -> (WriteOptionsPtr -> IO a) -> IO a
withCWriteOpts WriteOptions{..} = bracket mkCWriteOpts freeCWriteOpts
  where
    mkCWriteOpts = do
        opts_ptr <- c_rocksdb_writeoptions_create
        onException
            (c_rocksdb_writeoptions_set_sync opts_ptr $ boolToNum sync)
            (c_rocksdb_writeoptions_destroy opts_ptr)
        return opts_ptr

    freeCWriteOpts = c_rocksdb_writeoptions_destroy

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
        ccmp    <- c_rocksdb_comparator_create nullPtr cdest ccmpfun cname
        return $ Comparator' ccmpfun cdest cname ccmp


freeComparator :: Comparator' -> IO ()
freeComparator (Comparator' ccmpfun cdest cname ccmp) = do
    c_rocksdb_comparator_destroy ccmp
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
        cfp    <- c_rocksdb_filterpolicy_create nullPtr cdest ccffun ckmfun cname

        return $ FilterPolicy' ccffun ckmfun cdest cname cfp

freeFilterPolicy :: FilterPolicy' -> IO ()
freeFilterPolicy (FilterPolicy' ccffun ckmfun cdest cname cfp) = do
    c_rocksdb_filterpolicy_destroy cfp
    freeHaskellFunPtr ccffun
    freeHaskellFunPtr ckmfun
    freeHaskellFunPtr cdest
    freeHaskellFunPtr cname

mkCReadOpts :: ReadOptions -> IO ReadOptionsPtr
mkCReadOpts ReadOptions{..} = do
    opts_ptr <- c_rocksdb_readoptions_create
    flip onException (c_rocksdb_readoptions_destroy opts_ptr) $ do
        c_rocksdb_readoptions_set_verify_checksums opts_ptr $ boolToNum verifyCheckSums
        c_rocksdb_readoptions_set_fill_cache opts_ptr $ boolToNum fillCache

        case useSnapshot of
            Just (Snapshot snap_ptr) -> c_rocksdb_readoptions_set_snapshot opts_ptr snap_ptr
            Nothing -> return ()

    return opts_ptr

freeCReadOpts :: ReadOptionsPtr -> IO ()
freeCReadOpts = c_rocksdb_readoptions_destroy

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
