{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ea.Genesis
( -- * Genesis tx data
  Genesis(..)
, GChainId(..)

  -- * Devnet Genesis Txs
, development0
, development10
, development11
, development12
, development13
, development14
, development15
, development16
, development17
, development18
, development19
, developmentN

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

  -- * Coin Contract genesis
, coinContract
, fungibleAsset
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
    | Ten
    | Eleven
    | Twelve
    | Thirteen
    | Fourteen
    | Fifteen
    | Sixteen
    | Seventeen
    | Eighteen
    | Nineteen
    | N
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
    Ten -> "10"
    Eleven -> "11"
    Twelve -> "12"
    Thirteen -> "13"
    Fourteen -> "14"
    Fifteen -> "15"
    Sixteen -> "16"
    Seventeen -> "17"
    Eighteen -> "18"
    Nineteen -> "19"
    N -> "N"

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

coinContract :: FilePath
coinContract = "pact/coin-contract/load-coin-contract.yaml"

fungibleAsset :: FilePath
fungibleAsset = "pact/coin-contract/load-fungible-asset.yaml"

gasPayer :: FilePath
gasPayer = "pact/gas-payer/load-gas-payer.yaml"

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

development10 :: Genesis
development10 = development0
    & txChainId .~ Ten
    & coinbase .~ (Just dev10Grants)

development11 :: Genesis
development11 = development0
    & txChainId .~ Eleven
    & coinbase .~ (Just dev11Grants)

development12 :: Genesis
development12 = development0
    & txChainId .~ Twelve
    & coinbase .~ (Just dev12Grants)

development13 :: Genesis
development13 = development0
    & txChainId .~ Thirteen
    & coinbase .~ (Just dev13Grants)

development14 :: Genesis
development14 = development0
    & txChainId .~ Fourteen
    & coinbase .~ (Just dev14Grants)

development15 :: Genesis
development15 = development0
    & txChainId .~ Fifteen
    & coinbase .~ (Just dev15Grants)

development16 :: Genesis
development16 = development0
    & txChainId .~ Sixteen
    & coinbase .~ (Just dev16Grants)

development17 :: Genesis
development17 = development0
    & txChainId .~ Seventeen
    & coinbase .~ (Just dev17Grants)

development18 :: Genesis
development18 = development0
    & txChainId .~ Eighteen
    & coinbase .~ (Just dev18Grants)

development19 :: Genesis
development19 = development0
    & txChainId .~ Nineteen
    & coinbase .~ (Just dev19Grants)

devNs :: FilePath
devNs = "pact/genesis/ns.yaml"

devKeysets :: FilePath
devKeysets = "pact/genesis/devnet/keysets.yaml"

dev0Grants :: FilePath
dev0Grants = "pact/genesis/devnet/grants0.yaml"

dev10Grants :: FilePath
dev10Grants = "pact/genesis/devnet/grants10.yaml"

dev11Grants :: FilePath
dev11Grants = "pact/genesis/devnet/grants11.yaml"

dev12Grants :: FilePath
dev12Grants = "pact/genesis/devnet/grants12.yaml"

dev13Grants :: FilePath
dev13Grants = "pact/genesis/devnet/grants13.yaml"

dev14Grants :: FilePath
dev14Grants = "pact/genesis/devnet/grants14.yaml"

dev15Grants :: FilePath
dev15Grants = "pact/genesis/devnet/grants15.yaml"

dev16Grants :: FilePath
dev16Grants = "pact/genesis/devnet/grants16.yaml"

dev17Grants :: FilePath
dev17Grants = "pact/genesis/devnet/grants17.yaml"

dev18Grants :: FilePath
dev18Grants = "pact/genesis/devnet/grants18.yaml"

dev19Grants :: FilePath
dev19Grants = "pact/genesis/devnet/grants19.yaml"

devNGrants :: FilePath
devNGrants = "pact/genesis/devnet/grantsN.yaml"

devAllocations :: FilePath
devAllocations = "pact/genesis/devnet/allocations.yaml"


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
