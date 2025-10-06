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

import Chainweb.BlockHeader.Genesis.Development0Payload qualified as DN0
import Chainweb.BlockHeader.Genesis.Development1to19Payload qualified as DNN
import Chainweb.BlockHeader.Genesis.Mainnet0Payload qualified as MN0
import Chainweb.BlockHeader.Genesis.Mainnet10to19Payload qualified as MNKAD
import Chainweb.BlockHeader.Genesis.Mainnet1Payload qualified as MN1
import Chainweb.BlockHeader.Genesis.Mainnet2Payload qualified as MN2
import Chainweb.BlockHeader.Genesis.Mainnet3Payload qualified as MN3
import Chainweb.BlockHeader.Genesis.Mainnet4Payload qualified as MN4
import Chainweb.BlockHeader.Genesis.Mainnet5Payload qualified as MN5
import Chainweb.BlockHeader.Genesis.Mainnet6Payload qualified as MN6
import Chainweb.BlockHeader.Genesis.Mainnet7Payload qualified as MN7
import Chainweb.BlockHeader.Genesis.Mainnet8Payload qualified as MN8
import Chainweb.BlockHeader.Genesis.Mainnet9Payload qualified as MN9
import Chainweb.BlockHeader.Genesis.RecapDevelopment0Payload qualified as RDN0
import Chainweb.BlockHeader.Genesis.RecapDevelopment10to19Payload qualified as RDNKAD
import Chainweb.BlockHeader.Genesis.RecapDevelopment1to9Payload qualified as RDNN
import Chainweb.BlockHeader.Genesis.Testnet040Payload qualified as T04N0
import Chainweb.BlockHeader.Genesis.Testnet041to19Payload qualified as T04NN
import Chainweb.Pact.Payload
import Chainweb.Version
import Chainweb.Version.Development
import Chainweb.Version.EvmDevelopment
import Chainweb.Version.Mainnet
import Chainweb.Version.RecapDevelopment
import Chainweb.Version.Testnet04
import Control.Lens
import Data.HashMap.Strict qualified as HM
import GHC.Stack

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
