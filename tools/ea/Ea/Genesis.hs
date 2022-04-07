{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ea.Genesis
( -- * Genesis tx data
  Genesis(..)
, GChainId(..)

  -- * Devnet Genesis Txs
, development0
, developmentN
, developmentKAD

  -- * Devnet (testing) Genesis Txs
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
, fungibleAssetV1
, fungibleAssetV2
, fungibleXChainV1
, gasPayer
) where


import Control.Lens

import Data.Text

import Chainweb.Graph
import Chainweb.Version


-- ---------------------------------------------------------------------- --
-- Genesis Tx Data

-- | Type-safe representation of chain ids
-- for a 10-chain graph.
--
data GChainId
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | N
    | KAD
    deriving (Eq, Ord, Enum)

instance Show GChainId where
  show = \case
    Zero -> "0"
    One -> "1"
    Two -> "2"
    Three -> "3"
    Four -> "4"
    Five -> "5"
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine -> "9"
    N -> "N"
    KAD -> "KAD"

-- | Genesis transaction record
--
data Genesis = Genesis
  { _version :: ChainwebVersion
    -- ^ chainweb version (e.g. Testnet04)
  , _tag :: Text
    -- ^ Module name tag
  , _txChainId :: GChainId
    -- ^ chain id
  , _coinbase :: Maybe FilePath
    -- ^ filepath to coinbase yaml
  , _keysets :: Maybe FilePath
    -- ^ filepath to keyset yaml
  , _allocations :: Maybe FilePath
    -- ^ filepath to allocation yaml
  , _namespaces :: Maybe FilePath
    -- ^ filepath to namespace yaml
  } deriving (Eq, Ord, Show)

-- | A 'Lens'' into the transaction chain id
-- of a genesis payload
--
txChainId :: Lens' Genesis GChainId
txChainId = lens _txChainId (\t b -> t { _txChainId = b })

-- | A 'Lens'' into the transaction allocation filepath
-- of a genesis payload
--
allocations :: Lens' Genesis (Maybe FilePath)
allocations = lens _allocations (\t b -> t { _allocations = b })

-- | A 'Lens'' into the transaction allocation filepath
-- of a genesis payload
--
coinbase :: Lens' Genesis (Maybe FilePath)
coinbase = lens _coinbase (\t b -> t { _coinbase = b })

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
coinContractV2Install = "pact/coin-contract/v2/coin-install.pact"

coinContractV3 :: FilePath
coinContractV3 = "pact/coin-contract/v3/load-coin-contract-v3.yaml"

coinContractV4 :: FilePath
coinContractV4 = "pact/coin-contract/v4/load-coin-contract-v4.yaml"

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
    , _txChainId = Zero
    , _coinbase = Just dev0Grants
    , _keysets = Just devKeysets
    , _allocations = Just devAllocations
    , _namespaces = Just devNs
    }

developmentN :: Genesis
developmentN = development0
    & txChainId .~ N
    & coinbase .~ (Just devNGrants)

developmentKAD :: Genesis
developmentKAD = Genesis
    { _version = Development
    , _tag = "Development"
    , _txChainId = KAD
    , _coinbase = Just devnetKadOps
    , _keysets = Nothing
    , _allocations = Nothing
    , _namespaces = Just devNs
    }

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
    { _version = FastTimedCPM petersonChainGraph
    , _tag = "FastTimedCPM"
    , _txChainId = Zero
    , _coinbase = Just fast0Grants
    , _keysets = Just fastKeysets
    , _allocations = Just fastAllocations
    , _namespaces = Just fastNs
    }

fastTimedCPMN :: Genesis
fastTimedCPMN = fastTimedCPM0
    & txChainId .~ N
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
    , _txChainId = Zero
    , _coinbase = Just test0Grants
    , _keysets = Just testnetKeysets
    , _allocations = Just testnetAllocations
    , _namespaces = Just testNs
    }

testnetN :: Genesis
testnetN = testnet0
    & txChainId .~ N
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
    , _txChainId = Zero
    , _coinbase = Nothing
    , _keysets = Just mainnetKeysets
    , _allocations = Just mainnetAllocations0
    , _namespaces = Just mainNs
    }

mainnet1 :: Genesis
mainnet1 = mainnet0
    & txChainId .~ One
    & allocations .~ (Just mainnetAllocations1)

mainnet2 :: Genesis
mainnet2 = mainnet0
    & txChainId .~ Two
    & allocations .~ (Just mainnetAllocations2)

mainnet3 :: Genesis
mainnet3 = mainnet0
    & txChainId .~ Three
    & allocations .~ (Just mainnetAllocations3)

mainnet4 :: Genesis
mainnet4 = mainnet0
    & txChainId .~ Four
    & allocations .~ (Just mainnetAllocations4)

mainnet5 :: Genesis
mainnet5 = mainnet0
    & txChainId .~ Five
    & allocations .~ (Just mainnetAllocations5)

mainnet6 :: Genesis
mainnet6 = mainnet0
    & txChainId .~ Six
    & allocations .~ (Just mainnetAllocations6)

mainnet7 :: Genesis
mainnet7 = mainnet0
    & txChainId .~ Seven
    & allocations .~ (Just mainnetAllocations7)

mainnet8 :: Genesis
mainnet8 = mainnet0
    & txChainId .~ Eight
    & allocations .~ (Just mainnetAllocations8)

mainnet9 :: Genesis
mainnet9 = mainnet0
    & txChainId .~ Nine
    & allocations .~ (Just mainnetAllocations9)

mainnetKAD :: Genesis
mainnetKAD = Genesis
    { _version = Mainnet01
    , _tag = "Mainnet"
    , _txChainId = KAD
    , _coinbase = Just mainnetKadOps
    , _keysets = Nothing
    , _allocations = Nothing
    , _namespaces = Just mainNs
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
