{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Module: Chainweb.Version.Registry
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: MIT
-- Maintainer: Edmund Noble <edmund@kadena.io>
-- Stability: experimental
--
-- At certain points (in particular when decoding block headers) we need to be
-- able to look up ChainwebVersions by their version codes. We know of mainnet,
-- testnet, and devnet versions in prod code, but we don't know of testing
-- versions, and we also don't know if the user has enabled a flag that modifies
-- the devnet version, so we maintain a mutable registry mapping codes to
-- versions in this module.
--
module Chainweb.Version.Registry
    ( registerVersion
    , lookupVersionByCode
    , lookupVersionByName
    , fabricateVersionWithName
    , knownVersions
    , findKnownVersion
    , versionMap
    ) where

import Control.DeepSeq
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.IORef
import Data.Maybe
import qualified Data.Text as T
import System.IO.Unsafe

import GHC.Stack

import Chainweb.Version
import Chainweb.Version.Development
import Chainweb.Version.RecapDevelopment
import Chainweb.Version.Mainnet
import Chainweb.Version.Testnet
import Chainweb.Utils.Rule

{-# NOINLINE versionMap #-}
versionMap :: IORef (HashMap ChainwebVersionCode ChainwebVersion)
versionMap = unsafePerformIO $ do
    traverse_ validateVersion knownVersions
    newIORef $ HM.fromList [(_versionCode v, v) | v <- [mainnet, testnet]]

-- | Register a version into our registry by code, ensuring it contains no
-- errors and there are no others registered with that code.
registerVersion :: HasCallStack => ChainwebVersion -> IO ()
registerVersion v = do
    validateVersion v
    atomicModifyIORef' versionMap $ \m ->
        case HM.lookup (_versionCode v) m of
            Just v'
                | v /= v' -> error "registerVersion: conflicting version registered already"
                | otherwise -> (m, ())
            Nothing ->
                (HM.insert (_versionCode v) v m, ())

validateVersion :: HasCallStack => ChainwebVersion -> IO ()
validateVersion v = do
    evaluate (rnf v)
    let
        hasAllChains (AllChains _) = True
        hasAllChains (OnChains m) = HS.fromMap (void m) == chainIds v
        errors = concat
            [ [ "validateVersion: version does not have heights for all forks"
                | not (HS.fromMap (void $ _versionForks v) == HS.fromList [minBound :: Fork .. maxBound :: Fork]) ]
            , [ "validateVersion: version is missing fork heights for some forks on some chains"
                | not (all hasAllChains (_versionForks v)) ]
            , [ "validateVersion: chain graphs do not decrease in block height"
                | not (ruleValid (_versionGraphs v)) ]
            , [ "validateVersion: block gas limits do not decrease in block height"
                | not (ruleValid (_versionMaxBlockGasLimit v)) ]
            , [ "validateVersion: genesis data is missing for some chains"
                | not (and
                    [ hasAllChains (_genesisBlockPayload $ _versionGenesis v)
                    , hasAllChains (_genesisBlockTarget $ _versionGenesis v)
                    , hasAllChains (_genesisTime $ _versionGenesis v)
                    ])]
            ]
    unless (null errors) $
        error $ unlines $ ["errors encountered validating version " <> show v <> ":"] <> errors

-- | Look up a version in the registry by code.
lookupVersionByCode :: HasCallStack => ChainwebVersionCode -> ChainwebVersion
lookupVersionByCode code
    -- these two cases exist to ensure that the mainnet and testnet versions
    -- cannot be accidentally replaced and are the most performant to look up.
    -- registering them is still allowed, as long as they are not conflicting.
    | code == _versionCode mainnet = mainnet
    | code == _versionCode testnet = testnet
    | otherwise =
        -- Setting the version code here allows us to delay doing the lookup in
        -- the case that we don't actually need the version, just the code.
        lookupVersion & versionCode .~ code
  where
    lookupVersion = unsafeDupablePerformIO $ do
        m <- readIORef versionMap
        return $ fromMaybe (error notRegistered) $
            HM.lookup code m
    notRegistered
      | code == _versionCode recapDevnet = "recapDevnet version used but not registered, remember to do so after it's configured"
      | code == _versionCode devnet = "devnet version used but not registered, remember to do so after it's configured"
      | otherwise = "version not registered with code " <> show code <> ", have you seen Chainweb.Test.TestVersions.legalizeTestVersion?"

-- TODO: ideally all uses of this are deprecated. currently in use in
-- ObjectEncoded block header decoder and CutHashes decoder.
lookupVersionByName :: HasCallStack => ChainwebVersionName -> ChainwebVersion
lookupVersionByName name
    | name == _versionName mainnet = mainnet
    | name == _versionName testnet = testnet
    | otherwise = lookupVersion & versionName .~ name
  where
    lookupVersion = unsafeDupablePerformIO $ do
        m <- readIORef versionMap
        return $ fromMaybe (error notRegistered) $
            listToMaybe [ v | v <- HM.elems m, _versionName v == name ]
    notRegistered
      | name == _versionName recapDevnet = "recapDevnet version used but not registered, remember to do so after it's configured"
      | otherwise = "version not registered with name " <> show name <> ", have you seen Chainweb.Test.TestVersions.legalizeTestVersion?"

fabricateVersionWithName :: HasCallStack => ChainwebVersionName -> ChainwebVersion
fabricateVersionWithName name =
    error "attempted to access field of fabricated version." & versionName .~ name

-- | Versions known to us by name.
knownVersions :: [ChainwebVersion]
knownVersions = [mainnet, testnet, recapDevnet, devnet]

-- | Look up a known version by name, usually with `m` instantiated to some
-- configuration parser monad.
findKnownVersion :: MonadFail m => ChainwebVersionName -> m ChainwebVersion
findKnownVersion vn =
    case find (\v -> _versionName v == vn) knownVersions of
        Nothing -> fail $ T.unpack (getChainwebVersionName vn) <> " is not a known version: try development, mainnet01 or testnet04"
        Just v -> return v

instance HasChainwebVersion ChainwebVersionCode where
    _chainwebVersion = lookupVersionByCode
