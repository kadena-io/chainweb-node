{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ea.Genesis
( -- * Genesis tx data
  Genesis(..)
, ChainIdRange(..)
, chainIdRangeTag

  -- * Devnet Genesis Txs
, development0
, developmentN
, developmentKAD
, fastDevelopment0
, fastDevelopmentN

  -- * Testing Genesis Txs
, fastTimedCPM0
, fastTimedCPMN

  -- * Testnet Genesis txs
, testnet0
, testnetN

  -- * Mainnet Genesis txs
, mainnet0
, mainnet1
, mainnet2
, mainnet3
, mainnet4
, mainnet5
, mainnet6
, mainnet7
, mainnet8
, mainnet9
, mainnetKAD

  -- * Coin Contract genesis
, coinContractV1
, coinContractV2
, coinContractV2Install
, coinContractV3
, coinContractV4
, coinContractV5
, fungibleAssetV1
, fungibleAssetV2
, fungibleXChainV1
, gasPayer
) where


import Control.Lens

import Data.Text
import Data.Word

import Chainweb.Graph
import Chainweb.Test.TestVersions
import Chainweb.Version
import Chainweb.Version.Development
import Chainweb.Version.FastDevelopment
import Chainweb.Version.Mainnet
import Chainweb.Version.Testnet


-- ---------------------------------------------------------------------- --
-- Genesis Tx Data

-- A range of chain IDs [l,r]
data ChainIdRange
    = ChainIdRange !Word32 !Word32
    deriving (Eq, Ord)

chainIdRangeTag :: ChainIdRange -> String
chainIdRangeTag (ChainIdRange l u)
    | l == u = show l
    | otherwise = show l <> "to" <> show u

-- instance Show GChainId where
--   show = \case
--     Zero -> "0"
--     One -> "1"
--     Two -> "2"
--     Three -> "3"
--     Four -> "4"
--     Five -> "5"
--     Six -> "6"
--     Seven -> "7"
--     Eight -> "8"
--     Nine -> "9"
--     N -> "N"
--     KAD -> "KAD"

-- | Genesis transaction record
--
data Genesis = Genesis
    { _version :: ChainwebVersion
      -- ^ chainweb version (e.g. Testnet04)
    , _tag :: Text
      -- ^ Module name tag
    , _txChainIds :: ChainIdRange
      -- ^ chain id
    , _coinbase :: Maybe FilePath
      -- ^ filepath to coinbase yaml
    , _keysets :: Maybe FilePath
      -- ^ filepath to keyset yaml
    , _allocations :: Maybe FilePath
      -- ^ filepath to allocation yaml
    , _namespaces :: Maybe FilePath
      -- ^ filepath to namespace yaml
    , _coinContract :: [FilePath]
    } deriving (Eq, Ord) -- Show)

makeLenses ''Genesis

-- ---------------------------------------------------------------------- --
--  Coin Contract Essentials

coinContractV1 :: FilePath
coinContractV1 = "pact/coin-contract/load-coin-contract.yaml"

fungibleAssetV1 :: FilePath
fungibleAssetV1 = "pact/coin-contract/load-fungible-asset.yaml"

gasPayer :: FilePath
gasPayer = "pact/gas-payer/load-gas-payer.yaml"

coinContractV2 :: FilePath
coinContractV2 = "pact/coin-contract/v2/load-coin-contract-v2.yaml"

coinContractV2Install :: FilePath
coinContractV2Install = "pact/coin-contract/v2/install-coin-contract-v2.yaml"

coinContractV3 :: FilePath
coinContractV3 = "pact/coin-contract/v3/load-coin-contract-v3.yaml"

coinContractV4 :: FilePath
coinContractV4 = "pact/coin-contract/v4/load-coin-contract-v4.yaml"

coinContractV5 :: FilePath
coinContractV5 = "pact/coin-contract/v5/load-coin-contract-v5.yaml"

installCoinContractV5 :: FilePath
installCoinContractV5 = "pact/coin-contract/v5/install-coin-contract-v5.yaml"

fungibleAssetV2 :: FilePath
fungibleAssetV2 = "pact/coin-contract/v2/load-fungible-asset-v2.yaml"

fungibleXChainV1 :: FilePath
fungibleXChainV1 = "pact/coin-contract/v4/load-fungible-xchain-v1.yaml"

-- ---------------------------------------------------------------------- --
-- Devnet - Development

development0 :: Genesis
development0 = Genesis
    { _version = Development
    , _tag = "Development"
    , _txChainIds = ChainIdRange 0 0
    , _coinbase = Just dev0Grants
    , _keysets = Just devKeysets
    , _allocations = Just devAllocations
    , _namespaces = Just devNs
    , _coinContract = [fungibleAssetV1, coinContractV1, gasPayer]
    }

developmentN :: Genesis
developmentN = development0
    & txChainIds .~ ChainIdRange 1 9
    & coinbase .~ Just devNGrants

developmentKAD :: Genesis
developmentKAD = development0
    & txChainIds .~ ChainIdRange 10 19
    & coinbase .~ Just devnetKadOps
    & keysets .~ Nothing
    & allocations .~ Nothing
    & namespaces .~ Just devNs
    & coinContract .~ [fungibleAssetV1, fungibleAssetV2, coinContractV2Install, gasPayer]

fastDevelopment0 :: Genesis
fastDevelopment0 = Genesis
    { _version = FastDevelopment
    , _tag = "FastDevelopment"
    , _txChainIds = ChainIdRange 0 0
    , _coinbase = Just dev0Grants
    , _keysets = Just devKeysets
    , _allocations = Just devAllocations
    , _namespaces = Just devNs
    , _coinContract = [fungibleAssetV1, fungibleXChainV1, fungibleAssetV2, installCoinContractV5, gasPayer]
    }

fastDevelopmentN :: Genesis
fastDevelopmentN = fastDevelopment0
    & txChainIds .~ ChainIdRange 1 19
    & coinbase .~ (Just devNGrants)

devNs :: FilePath
devNs = "pact/genesis/ns.yaml"

devKeysets :: FilePath
devKeysets = "pact/genesis/devnet/keysets.yaml"

dev0Grants :: FilePath
dev0Grants = "pact/genesis/devnet/grants0.yaml"

devNGrants :: FilePath
devNGrants = "pact/genesis/devnet/grantsN.yaml"

devAllocations :: FilePath
devAllocations = "pact/genesis/devnet/allocations.yaml"

devnetKadOps :: FilePath
devnetKadOps = "pact/genesis/devnet/kad-ops-grants.yaml"

-- ---------------------------------------------------------------------- --
-- Fast timed CPM

fastTimedCPM0 :: Genesis
fastTimedCPM0 = Genesis
    { _version = fastForkingCpmTestVersion petersonChainGraph
    , _tag = "FastTimedCPM"
    , _txChainIds = ChainIdRange 0 0
    , _coinbase = Just fast0Grants
    , _keysets = Just fastKeysets
    , _allocations = Just fastAllocations
    , _namespaces = Just fastNs
    , _coinContract = [fungibleAssetV1, coinContractV1, gasPayer]
    }

fastTimedCPMN :: Genesis
fastTimedCPMN = fastTimedCPM0
    & txChainIds .~ ChainIdRange 1 9
    & coinbase .~ (Just fastNGrants)

fastNs :: FilePath
fastNs = "pact/genesis/ns.yaml"

fastKeysets :: FilePath
fastKeysets = "pact/genesis/devnet/keysets.yaml"

fast0Grants :: FilePath
fast0Grants = "pact/genesis/devnet/grants0.yaml"

fastNGrants :: FilePath
fastNGrants = "pact/genesis/devnet/grantsN.yaml"

fastAllocations :: FilePath
fastAllocations = "pact/genesis/devnet/allocations.yaml"

-- ---------------------------------------------------------------------- --
-- Testnet

testnet0 :: Genesis
testnet0 = Genesis
    { _version = Testnet04
    , _tag = "Testnet"
    , _txChainIds = ChainIdRange 0 0
    , _coinbase = Just test0Grants
    , _keysets = Just testnetKeysets
    , _allocations = Just testnetAllocations
    , _namespaces = Just testNs
    , _coinContract = [fungibleAssetV1, coinContractV1, gasPayer]
    }

testnetN :: Genesis
testnetN = testnet0
    & txChainIds .~ ChainIdRange 1 19
    & coinbase .~ (Just testNGrants)

test0Grants :: FilePath
test0Grants = "pact/genesis/testnet/grants0.yaml"

testNGrants :: FilePath
testNGrants = "pact/genesis/testnet/grantsN.yaml"

testNs :: FilePath
testNs = "pact/genesis/ns.yaml"

testnetAllocations :: FilePath
testnetAllocations = "pact/genesis/testnet/allocations.yaml"

testnetKeysets :: FilePath
testnetKeysets = "pact/genesis/testnet/keysets.yaml"

-- ---------------------------------------------------------------------- --
-- Mainnet

mainnet0 :: Genesis
mainnet0 = Genesis
    { _version = Mainnet01
    , _tag = "Mainnet"
    , _txChainIds = ChainIdRange 0 0
    , _coinbase = Nothing
    , _keysets = Just mainnetKeysets
    , _allocations = Just mainnetAllocations0
    , _namespaces = Just mainNs
    , _coinContract = [fungibleAssetV1, coinContractV1, gasPayer]
    }

mainnet1 :: Genesis
mainnet1 = mainnet0
    & txChainIds .~ ChainIdRange 1 1
    & allocations .~ (Just mainnetAllocations1)

mainnet2 :: Genesis
mainnet2 = mainnet0
    & txChainIds .~ ChainIdRange 2 2
    & allocations .~ (Just mainnetAllocations2)

mainnet3 :: Genesis
mainnet3 = mainnet0
    & txChainIds .~ ChainIdRange 3 3
    & allocations .~ (Just mainnetAllocations3)

mainnet4 :: Genesis
mainnet4 = mainnet0
    & txChainIds .~ ChainIdRange 4 4
    & allocations .~ (Just mainnetAllocations4)

mainnet5 :: Genesis
mainnet5 = mainnet0
    & txChainIds .~ ChainIdRange 5 5
    & allocations .~ (Just mainnetAllocations5)

mainnet6 :: Genesis
mainnet6 = mainnet0
    & txChainIds .~ ChainIdRange 6 6
    & allocations .~ (Just mainnetAllocations6)

mainnet7 :: Genesis
mainnet7 = mainnet0
    & txChainIds .~ ChainIdRange 7 7
    & allocations .~ (Just mainnetAllocations7)

mainnet8 :: Genesis
mainnet8 = mainnet0
    & txChainIds .~ ChainIdRange 8 8
    & allocations .~ (Just mainnetAllocations8)

mainnet9 :: Genesis
mainnet9 = mainnet0
    & txChainIds .~ ChainIdRange 9 9
    & allocations .~ (Just mainnetAllocations9)

mainnetKAD :: Genesis
mainnetKAD = Genesis
    { _version = Mainnet01
    , _tag = "Mainnet"
    , _txChainIds = ChainIdRange 10 19
    , _coinbase = Just mainnetKadOps
    , _keysets = Nothing
    , _allocations = Nothing
    , _namespaces = Just mainNs
    , _coinContract = [fungibleAssetV1, fungibleAssetV2, coinContractV2Install, gasPayer]
    }

mainnetKadOps :: FilePath
mainnetKadOps = "pact/genesis/mainnet/kad-ops-grants.yaml"

mainNs :: FilePath
mainNs = "pact/genesis/mainnet/ns.yaml"

mainnetAllocations0 :: FilePath
mainnetAllocations0 = "pact/genesis/mainnet/mainnet_allocations0.yaml"

mainnetAllocations1 :: FilePath
mainnetAllocations1 = "pact/genesis/mainnet/mainnet_allocations1.yaml"

mainnetAllocations2 :: FilePath
mainnetAllocations2 = "pact/genesis/mainnet/mainnet_allocations2.yaml"

mainnetAllocations3 :: FilePath
mainnetAllocations3 = "pact/genesis/mainnet/mainnet_allocations3.yaml"

mainnetAllocations4 :: FilePath
mainnetAllocations4 = "pact/genesis/mainnet/mainnet_allocations4.yaml"

mainnetAllocations5 :: FilePath
mainnetAllocations5 = "pact/genesis/mainnet/mainnet_allocations5.yaml"

mainnetAllocations6 :: FilePath
mainnetAllocations6 = "pact/genesis/mainnet/mainnet_allocations6.yaml"

mainnetAllocations7 :: FilePath
mainnetAllocations7 = "pact/genesis/mainnet/mainnet_allocations7.yaml"

mainnetAllocations8 :: FilePath
mainnetAllocations8 = "pact/genesis/mainnet/mainnet_allocations8.yaml"

mainnetAllocations9 :: FilePath
mainnetAllocations9 = "pact/genesis/mainnet/mainnet_allocations9.yaml"

mainnetKeysets :: FilePath
mainnetKeysets = "pact/genesis/mainnet/mainnet_keysets.yaml"
