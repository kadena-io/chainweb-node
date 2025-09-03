{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- Module: Chainweb.PayloadProvider.Pact.Genesis
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.PayloadProvider.Pact.Genesis
( genesisPayload
) where

import Chainweb.Version
import Chainweb.Version.Development
import Chainweb.Version.EvmDevelopment
import Chainweb.Version.Mainnet
import Chainweb.Version.RecapDevelopment
import Chainweb.Version.Testnet04
import Chainweb.Pact.Payload
import Control.Lens
import Data.HashMap.Strict qualified as HM
import GHC.Stack

import qualified Chainweb.BlockHeader.Genesis.Mainnet0Payload as MN0
import qualified Chainweb.BlockHeader.Genesis.Mainnet1Payload as MN1
import qualified Chainweb.BlockHeader.Genesis.Mainnet2Payload as MN2
import qualified Chainweb.BlockHeader.Genesis.Mainnet3Payload as MN3
import qualified Chainweb.BlockHeader.Genesis.Mainnet4Payload as MN4
import qualified Chainweb.BlockHeader.Genesis.Mainnet5Payload as MN5
import qualified Chainweb.BlockHeader.Genesis.Mainnet6Payload as MN6
import qualified Chainweb.BlockHeader.Genesis.Mainnet7Payload as MN7
import qualified Chainweb.BlockHeader.Genesis.Mainnet8Payload as MN8
import qualified Chainweb.BlockHeader.Genesis.Mainnet9Payload as MN9
import qualified Chainweb.BlockHeader.Genesis.Mainnet10to19Payload as MNKAD
import qualified Chainweb.BlockHeader.Genesis.Development0Payload as DN0
import qualified Chainweb.BlockHeader.Genesis.Development1to19Payload as DNN
import qualified Chainweb.BlockHeader.Genesis.Testnet040Payload as T04N0
import qualified Chainweb.BlockHeader.Genesis.Testnet041to19Payload as T04NN
import qualified Chainweb.BlockHeader.Genesis.RecapDevelopment0Payload as RDN0
import qualified Chainweb.BlockHeader.Genesis.RecapDevelopment1to9Payload as RDNN
import qualified Chainweb.BlockHeader.Genesis.RecapDevelopment10to19Payload as RDNKAD
import Chainweb.Utils

genesisPayload
    :: (HasCallStack, HasVersion)
    => ChainId -> Maybe PayloadWithOutputs
genesisPayload cid
    | _versionCode implicitVersion == _versionCode Mainnet01 =
        mainnetPayloads ^. at cid
    | _versionCode implicitVersion == _versionCode Testnet04 =
        testnet04Payloads ^. at cid
    | _versionCode implicitVersion == _versionCode Development =
        devnetPayloads ^. at cid
    | _versionCode implicitVersion == _versionCode RecapDevelopment =
        recapDevnetPayloads ^. at cid
    | _versionCode implicitVersion == _versionCode EvmDevelopment =
        evmDevnetPayloads ^. at cid
    | otherwise = Nothing
    where
    mainnetPayloads = ChainMap $ HM.fromList $ concat
        [
            [ (unsafeChainId 0, MN0.payloadBlock)
            , (unsafeChainId 1, MN1.payloadBlock)
            , (unsafeChainId 2, MN2.payloadBlock)
            , (unsafeChainId 3, MN3.payloadBlock)
            , (unsafeChainId 4, MN4.payloadBlock)
            , (unsafeChainId 5, MN5.payloadBlock)
            , (unsafeChainId 6, MN6.payloadBlock)
            , (unsafeChainId 7, MN7.payloadBlock)
            , (unsafeChainId 8, MN8.payloadBlock)
            , (unsafeChainId 9, MN9.payloadBlock)
            ]
        , [(unsafeChainId i, MNKAD.payloadBlock) | i <- [10..19]]
        ]
    testnet04Payloads = ChainMap $ HM.fromList $ concat
        [ [(unsafeChainId 0, T04N0.payloadBlock)]
        , [(unsafeChainId i, T04NN.payloadBlock) | i <- [1..19]]
        ]
    devnetPayloads = ChainMap $ HM.fromList $ concat
        [ [(unsafeChainId 0, DN0.payloadBlock)]
        , [(unsafeChainId i, DNN.payloadBlock) | i <- [1..19]]
        ]
    recapDevnetPayloads = ChainMap $ HM.fromList $ concat
        [ [(unsafeChainId 0, RDN0.payloadBlock)]
        , [(unsafeChainId i, RDNN.payloadBlock) | i <- [1..9]]
        , [(unsafeChainId i, RDNKAD.payloadBlock) | i <- [10..19]]
        ]
    evmDevnetPayloads = ChainMap $ HM.fromList $ concat
        [ [(unsafeChainId 0, DN0.payloadBlock)]
        , [(unsafeChainId i, DNN.payloadBlock) | i <- [1..19]]
        ]
