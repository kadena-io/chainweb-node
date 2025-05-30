{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
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
-- Be careful in this module. We hope to be able to delete it eventually,
-- because it works badly with tests.
--
module Chainweb.Version.Registry
    ( validateVersion
    , knownVersions
    , findKnownVersion
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
import Chainweb.Version.EvmDevelopment
import Chainweb.Version.EvmDevelopmentSingleton
import Chainweb.Version.RecapDevelopment
import Chainweb.Version.Mainnet
import Chainweb.Version.Testnet04
import Chainweb.Utils.Rule
-- temporarily left off because it doesn't validate

validateVersion :: HasCallStack => ChainwebVersion -> IO ()
validateVersion v = do
    evaluate (rnf v)
    let
        hasAllChains :: ChainMap a -> Bool
        hasAllChains (ChainMap m) = HS.fromMap (void m) == withVersion v chainIds
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
            , [ "validateVersion: some pact upgrade has no transactions"
                | any (any isUpgradeEmpty) (_versionUpgrades v) ]
            -- TODO: check that pact 4/5 upgrades are only enabled when pact 4/5 is enabled
            ]
    unless (null errors) $
        error $ unlines $ ["errors encountered validating version", show v] <> errors
    where
    isUpgradeEmpty PactUpgrade{_pactUpgradeTransactions = upg} = null upg

-- | Versions known to us by name.
knownVersions :: [ChainwebVersion]
knownVersions = [mainnet, testnet04, recapDevnet, devnet, evmDevnet, evmDevnetSingleton]

-- | Look up a known version by name, usually with `m` instantiated to some
-- configuration parser monad.
findKnownVersion :: MonadFail m => ChainwebVersionName -> m ChainwebVersion
findKnownVersion vn =
    case find (\v -> _versionName v == vn) knownVersions of
        Nothing -> fail $
            T.unpack (getChainwebVersionName vn) <> " is not a known version. perhaps you meant one of these:\n"
            <> show (getChainwebVersionName . _versionName <$> knownVersions)
        Just v -> return v
