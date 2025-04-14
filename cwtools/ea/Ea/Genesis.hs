{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Ea.Genesis
( -- * Genesis tx data
  Genesis(..)
, ChainIdRange
, pattern ChainIdRange
, mkChainIdRange
, onlyChainId
, chainIdRangeTag

  -- * Devnet Genesis Txs
, recapDevelopment0
, recapDevelopmentN
, recapDevelopmentKAD
, fastDevelopment0
, fastDevelopmentN

  -- * Testing Genesis Txs
, fastTimedCPM0
, fastTimedCPMN
, instantCPM0
, instantCPMN
, pact5InstantCPM0
, pact5InstantCPMN
, quirkedPact5InstantCPM0
, quirkedPact5InstantCPMN

  -- * Testnet Genesis txs
, testnet040
, testnet04N

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
, coinContractV6
, fungibleAssetV1
, fungibleAssetV2
, fungibleXChainV1
, gasPayer
) where


import Control.Lens
import Control.Monad

import Data.Text
import Data.Word

import Chainweb.Graph
import Chainweb.Test.TestVersions
import Chainweb.Version
import Chainweb.Version.Development
import Chainweb.Version.RecapDevelopment
import Chainweb.Version.Mainnet
import Chainweb.Version.Testnet04


-- ---------------------------------------------------------------------- --
-- Genesis Tx Data

-- A range of chain IDs [l,r]
data ChainIdRange
    = UnsafeChainIdRange !Word32 !Word32
    deriving (Eq, Ord)

{-# COMPLETE ChainIdRange #-}
pattern ChainIdRange :: Word32 -> Word32 -> ChainIdRange
pattern ChainIdRange l r <- UnsafeChainIdRange l r

mkChainIdRange :: Word32 -> Word32 -> ChainIdRange
mkChainIdRange l u
  | l <= u = UnsafeChainIdRange l u
  | otherwise = error "mkChainIdRange: chain IDs are not in order"

onlyChainId :: Word32 -> ChainIdRange
onlyChainId = join mkChainIdRange

chainIdRangeTag :: ChainIdRange -> String
chainIdRangeTag (ChainIdRange l u)
    | l == u = show l
    | otherwise = show l <> "to" <> show u

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
    } deriving (Eq, Ord)

makeLensesFor [("_" <> fn, fn) | fn <- ["txChainIds", "coinbase", "keysets", "allocations", "namespaces", "coinContract"]] ''Genesis

-- ---------------------------------------------------------------------- --
--  Coin Contract Essentials

coinContractV1 :: FilePath
coinContractV1 = "pact/coin-contract/v1/load-coin-contract.yaml"

fungibleAssetV1 :: FilePath
fungibleAssetV1 = "pact/coin-contract/v1/load-fungible-asset.yaml"

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

coinContractV6 :: FilePath
coinContractV6 = "pact/coin-contract/load-coin-contract.yaml"

installCoinContractV6 :: FilePath
installCoinContractV6 = "pact/coin-contract/install-coin-contract.yaml"

fungibleAssetV2 :: FilePath
fungibleAssetV2 = "pact/coin-contract/v2/load-fungible-asset-v2.yaml"

fungibleXChainV1 :: FilePath
fungibleXChainV1 = "pact/coin-contract/v4/load-fungible-xchain-v1.yaml"

-- ---------------------------------------------------------------------- --
-- Devnet - RecapDevelopment

recapDevelopment0 :: Genesis
recapDevelopment0 = Genesis
    { _version = RecapDevelopment
    , _tag = "RecapDevelopment"
    , _txChainIds = onlyChainId 0
    , _coinbase = Just dev0Grants
    , _keysets = Just devKeysets
    , _allocations = Just devAllocations
    , _namespaces = Just devNs
    , _coinContract = [fungibleAssetV1, coinContractV1, gasPayer]
    }

recapDevelopmentN :: Genesis
recapDevelopmentN = recapDevelopment0
    & txChainIds .~ mkChainIdRange 1 9
    & coinbase ?~ devNGrants

recapDevelopmentKAD :: Genesis
recapDevelopmentKAD = recapDevelopment0
    & txChainIds .~ mkChainIdRange 10 19
    & coinbase ?~ devnetKadOps
    & keysets .~ Nothing
    & allocations .~ Nothing
    & namespaces ?~ devNs
    & coinContract .~ [fungibleAssetV1, fungibleAssetV2, coinContractV2Install, gasPayer]

-- ---------------------------------------------------------------------- --
-- Devnet - Development

fastDevelopment0 :: Genesis
fastDevelopment0 = Genesis
    { _version = Development
    , _tag = "Development"
    , _txChainIds = onlyChainId 0
    , _coinbase = Just dev0Grants
    , _keysets = Just devKeysets
    , _allocations = Just devAllocations
    , _namespaces = Just devNs2
    , _coinContract = [fungibleAssetV1, fungibleXChainV1, fungibleAssetV2, installCoinContractV6, gasPayer]
    }

fastDevelopmentN :: Genesis
fastDevelopmentN = fastDevelopment0
    & txChainIds .~ mkChainIdRange 1 19
    & coinbase ?~ devNGrants

devNs2 :: FilePath
devNs2 = "pact/genesis/ns-v2.yaml"

devNs :: FilePath
devNs = "pact/genesis/ns-v1.yaml"

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
-- CPM test versions

instantCPM0 :: Genesis
instantCPM0 = Genesis
    { _version = instantCpmTestVersion petersenChainGraph
    , _tag = "InstantTimedCPM"
    , _txChainIds = onlyChainId 0
    , _coinbase = Just fast0Grants
    , _keysets = Just fastKeysets
    , _allocations = Just fastAllocations
    , _namespaces = Just devNs2
    , _coinContract = [fungibleAssetV1, fungibleXChainV1, fungibleAssetV2, installCoinContractV6, gasPayer]
    }

instantCPMN :: Genesis
instantCPMN = instantCPM0
  & txChainIds .~ mkChainIdRange 1 9
  & coinbase ?~ fastNGrants

pact5InstantCPM0 :: Genesis
pact5InstantCPM0 = Genesis
    { _version = pact5InstantCpmTestVersion petersenChainGraph
    , _tag = "Pact5InstantTimedCPM"
    , _txChainIds = onlyChainId 0
    , _coinbase = Just fast0Grants
    , _keysets = Just fastKeysets
    , _allocations = Just fastAllocations
    , _namespaces = Just devNs2
    , _coinContract = [fungibleAssetV1, fungibleXChainV1, fungibleAssetV2, installCoinContractV6, gasPayer]
    }

pact5InstantCPMN :: Genesis
pact5InstantCPMN = pact5InstantCPM0
  & txChainIds .~ mkChainIdRange 1 9
  & coinbase ?~ fastNGrants

quirkedPact5InstantCPM0 :: Genesis
quirkedPact5InstantCPM0 = Genesis
    { _version = quirkedGasPact5InstantCpmTestVersion petersenChainGraph
    , _tag = "QuirkedGasPact5InstantTimedCPM"
    , _txChainIds = onlyChainId 0
    , _coinbase = Just fast0Grants
    , _keysets = Just fastKeysets
    , _allocations = Just fastAllocations
    , _namespaces = Just devNs2
    , _coinContract = [fungibleAssetV1, fungibleXChainV1, fungibleAssetV2, installCoinContractV6, gasPayer]
    }

quirkedPact5InstantCPMN :: Genesis
quirkedPact5InstantCPMN = quirkedPact5InstantCPM0
  & txChainIds .~ mkChainIdRange 1 9
  & coinbase ?~ fastNGrants

fastTimedCPM0 :: Genesis
fastTimedCPM0 = Genesis
    { _version = fastForkingCpmTestVersion petersenChainGraph
    , _tag = "FastTimedCPM"
    , _txChainIds = onlyChainId 0
    , _coinbase = Just fast0Grants
    , _keysets = Just fastKeysets
    , _allocations = Just fastAllocations
    , _namespaces = Just fastNs
    , _coinContract = [fungibleAssetV1, coinContractV1, gasPayer]
    }

fastTimedCPMN :: Genesis
fastTimedCPMN = fastTimedCPM0
    & txChainIds .~ mkChainIdRange 1 9
    & coinbase ?~ fastNGrants

fastNs :: FilePath
fastNs = "pact/genesis/ns-v1.yaml"

fastKeysets :: FilePath
fastKeysets = "pact/genesis/devnet/keysets.yaml"

fast0Grants :: FilePath
fast0Grants = "pact/genesis/devnet/grants0.yaml"

fastNGrants :: FilePath
fastNGrants = "pact/genesis/devnet/grantsN.yaml"

fastAllocations :: FilePath
fastAllocations = "pact/genesis/devnet/allocations.yaml"

-- ---------------------------------------------------------------------- --
-- Testnet 04

testnet040 :: Genesis
testnet040 = Genesis
    { _version = Testnet04
    , _tag = "Testnet04"
    , _txChainIds = onlyChainId 0
    , _coinbase = Just test0Grants
    , _keysets = Just testnetKeysets
    , _allocations = Just testnetAllocations
    , _namespaces = Just testNs
    , _coinContract = [fungibleAssetV1, coinContractV1, gasPayer]
    }

testnet04N :: Genesis
testnet04N = testnet040
    & txChainIds .~ mkChainIdRange 1 19
    & coinbase ?~ testNGrants

test0Grants :: FilePath
test0Grants = "pact/genesis/testnet04/grants0.yaml"

testNGrants :: FilePath
testNGrants = "pact/genesis/testnet04/grantsN.yaml"

testNs :: FilePath
testNs = "pact/genesis/ns-v1.yaml"

testnetAllocations :: FilePath
testnetAllocations = "pact/genesis/testnet04/allocations.yaml"

testnetKeysets :: FilePath
testnetKeysets = "pact/genesis/testnet04/keysets.yaml"

-- ---------------------------------------------------------------------- --
-- Mainnet

mainnet0 :: Genesis
mainnet0 = Genesis
    { _version = Mainnet01
    , _tag = "Mainnet"
    , _txChainIds = onlyChainId 0
    , _coinbase = Nothing
    , _keysets = Just mainnetKeysets
    , _allocations = Just mainnetAllocations0
    , _namespaces = Just mainNs
    , _coinContract = [fungibleAssetV1, coinContractV1, gasPayer]
    }

mainnet1 :: Genesis
mainnet1 = mainnet0
    & txChainIds .~ onlyChainId 1
    & allocations ?~ mainnetAllocations1

mainnet2 :: Genesis
mainnet2 = mainnet0
    & txChainIds .~ onlyChainId 2
    & allocations ?~ mainnetAllocations2

mainnet3 :: Genesis
mainnet3 = mainnet0
    & txChainIds .~ onlyChainId 3
    & allocations ?~ mainnetAllocations3

mainnet4 :: Genesis
mainnet4 = mainnet0
    & txChainIds .~ onlyChainId 4
    & allocations ?~ mainnetAllocations4

mainnet5 :: Genesis
mainnet5 = mainnet0
    & txChainIds .~ onlyChainId 5
    & allocations ?~ mainnetAllocations5

mainnet6 :: Genesis
mainnet6 = mainnet0
    & txChainIds .~ onlyChainId 6
    & allocations ?~ mainnetAllocations6

mainnet7 :: Genesis
mainnet7 = mainnet0
    & txChainIds .~ onlyChainId 7
    & allocations ?~ mainnetAllocations7

mainnet8 :: Genesis
mainnet8 = mainnet0
    & txChainIds .~ onlyChainId 8
    & allocations ?~ mainnetAllocations8

mainnet9 :: Genesis
mainnet9 = mainnet0
    & txChainIds .~ onlyChainId 9
    & allocations ?~ mainnetAllocations9

mainnetKAD :: Genesis
mainnetKAD = Genesis
    { _version = Mainnet01
    , _tag = "Mainnet"
    , _txChainIds = mkChainIdRange 10 19
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
