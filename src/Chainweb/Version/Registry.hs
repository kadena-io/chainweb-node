{-# language RecordWildCards #-}

module Chainweb.Version.Registry
    ( registerVersion
    , validateVersion
    , lookupVersionByCode
    , knownVersions
    , findKnownVersion
    , versionMap
    ) where

import Control.DeepSeq
import Control.Exception
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Maybe
import qualified Data.Text as T
import System.IO.Unsafe

import GHC.Stack

import Chainweb.Version
import Chainweb.Version.Development
import Chainweb.Version.Mainnet
import Chainweb.Version.Testnet

{-# NOINLINE versionMap #-}
versionMap :: IORef (HashMap ChainwebVersionCode ChainwebVersion)
versionMap = unsafePerformIO $ do
    traverse_ validateVersion knownVersions
    newIORef HM.empty

-- disallow duplicates
registerVersion :: ChainwebVersion -> IO ()
registerVersion v = do
    validateVersion v
    atomicModifyIORef' versionMap $ \m ->
        case HM.lookup (_versionCode v) m of
            Just v'
                | v /= v' -> error "registerVersion: conflicting version registered already"
                | otherwise -> (m, ())
            Nothing ->
                (HM.insert (_versionCode v) v m, ())

validateVersion :: ChainwebVersion -> IO ()
validateVersion v = do -- edtodo
    evaluate (rnf v)

-- edtodo: doc
lookupVersionByCode :: HasCallStack => ChainwebVersionCode -> ChainwebVersion
lookupVersionByCode code
    | Just v <- find (\v -> _versionCode v == code) knownVersions = v
    | otherwise = lazify (unsafeDupablePerformIO $ do
        m <- readIORef versionMap
        return $ fromMaybe (error $ "version not registered with code " <> show code) $ HM.lookup code m
        ) { _versionCode = code }
  where
    lazify ~ChainwebVersion{..} = ChainwebVersion{..}

knownVersions :: [ChainwebVersion]
knownVersions = [mainnet, testnet, devnet]

findKnownVersion :: MonadFail m => ChainwebVersionName -> m ChainwebVersion
findKnownVersion vn =
    case find (\v -> _versionName v == vn) knownVersions of
        Nothing -> fail $ T.unpack (getChainwebVersionName vn) <> " is not a known version: try development, mainnet01 or testnet04"
        Just v -> return v
