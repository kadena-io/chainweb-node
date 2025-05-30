{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: evm-genesis.Main
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Main
( main
) where

import Chainweb.PayloadProvider.EVM.EngineAPI qualified as E
import Chainweb.PayloadProvider.EVM.EthRpcAPI qualified as E
import Chainweb.PayloadProvider.EVM.Header qualified as E
import Chainweb.PayloadProvider.EVM.JsonRPC qualified as E
import Chainweb.Utils
import Control.Monad
import Data.Aeson
import Data.String
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Ethereum.Misc qualified as E
import Ethereum.RLP qualified as E
import Network.HTTP.Client qualified as HTTP
import Network.URI
import Numeric.Natural
import System.Directory
import System.Environment
import System.IO
import System.Process
import Text.Printf
import Control.Retry

-- -------------------------------------------------------------------------- --
-- Main

-- | This program generates chain-spec files, genesis blocks, and the respective
-- block hashes for EVMs on Chainweb networks.
--
-- The results can be used to define the genesis information for the respective
-- networks in the chainweb-node code basis.
--
main :: IO ()
main = do

    -- parse command line
    (n, cids, spec) <- getArgs >>= \case
        [] -> error "No argument for the chainweb version provided. The version must be one of: 'mainnet', 'testnet', 'evm-testnet', or 'evm-development'."
        ["mainnet"] -> do
            let cids = [20..25]
            return ("mainnet", cids, mainnetSpecFile)
        ["testnet"] -> do
            let cids = [20..25]
            return ("testnet", cids, testnetSpecFile)
        ["evm-testnet"] -> do
            let cids = [20..25]
            return ("evm-testnet", cids, evmTestnetSpecFile)
        ["evm-development"] -> do
            let cids = [20..25]
            return ("evm-development", cids, evmDevnetSpecFile)
        ["evm-development-singleton"] -> do
            let cids = [20]
            return ("evm-development-singleton", cids, evmDevnetSpecFile)
        _ -> error "Invalid argument for the chainweb version provided. The version must be one of: 'mainnet', 'testnet', 'evm-testnet', or 'evm-development'."

    hdrs <- forM cids $ \cid -> do
        let specFileDir = "./chain-specs/" <> n
        createDirectoryIfMissing True specFileDir
        let specFileName = specFileDir <> "/chain-spec-" <> show cid <> ".json"
        encodeFile specFileName $ spec cid
        hdr <- queryNode cid specFileName
        return (cid, hdr)

    T.putStrLn $ encodeToText
        [ object
            [ "chainId" .= cid
            , "blockPayloadHash" .= E._hdrPayloadHash hdr
            , "blockPayload" .= encodeB64UrlNoPaddingText (E.putRlpByteString hdr)
            , "evmPayloadHeader" .= hdr
            ]
        | (cid, hdr) <- hdrs
        ]

-- -------------------------------------------------------------------------- --
-- Querying the Node

queryNode :: Natural -> FilePath -> IO E.Header
queryNode cid spec = withRethNode cid spec $ \uri -> do
    ctx <- mkRpcCtx uri
    hdr <- getBlockAtNumber ctx 0
    return hdr

-- | Run reth node with chain-spec file at default port 8545 and execute an
-- action with the node URI.
--
withRethNode :: Natural -> FilePath -> (URI -> IO a) -> IO a
withRethNode cid specfile act =
    withCreateProcess runProc $ \_stdin _stdout _stderr _ph -> do
        recoverAll policy $ \_ -> do
            hPutStrLn stderr $ "Waiting for reth node for chain " <> show cid <> " to start..."
            uri <- fromText (fromString ("http://localhost:" <> rethPort))
            r <- act uri
            hFlush stdout
            return r
  where
    policy = constantDelay 500_000 <> limitRetries 20
    runProc = proc cmd (dockerArgs <> rethArgs)
    cmd = "docker"
    dockerArgs =
        [ "run"
        , "--rm"
        , "-t"
        , "-p", rethPort <> ":" <> rethPort
        , "--volume=" <> specfile <> ":/spec.json"
        , "ghcr.io/kadena-io/kadena-reth:latest"
        ]
    rethArgs =
        [ "-q"
        , "node"
        , "--chain=/spec.json"
        , "--http"
        , "--http.addr=0.0.0.0"
        , "--http.port=" <> rethPort
        , "--http.api=eth,rpc"
        ]
    -- we use different ports for each reth to avoid conflicts when ports are
    -- not becoming available again fast enough
    rethPort = show (8545 + cid)

getBlockAtNumber
    :: E.JsonRpcHttpCtx
    -> E.BlockNumber
    -> IO E.Header
getBlockAtNumber ctx n = do
    r <- E.callMethodHttp
        @E.Eth_GetBlockByNumber ctx (E.DefaultBlockNumber n, False)
    case r of
        Just h -> return h
        Nothing -> error $ "Block not found: " <> show (E.DefaultBlockNumber n)

mkRpcCtx :: URI -> IO E.JsonRpcHttpCtx
mkRpcCtx u = do
    mgr <- HTTP.newManager HTTP.defaultManagerSettings
    return $ E.JsonRpcHttpCtx
        { E._jsonRpcHttpCtxManager = mgr
        , E._jsonRpcHttpCtxURI = u
        , E._jsonRpcHttpCtxMakeBearerToken = Nothing
        }

-- -------------------------------------------------------------------------- --
-- Spec File For EVM Devnet

-- | Intended only for local development and testing. It is not intended for use
-- with public networks.
--
-- The keys for all allocations are publicly known.
--
-- The Ethereum network chain ids are not officially registered and overlap with
-- the chain ids of other networks.
--
-- The configuration of the network may change at any time.
--
-- EVMs are available at block height 0.
--
evmDevnetSpecFile
    :: Natural
        -- numeric chainweb chain id
    -> Value
evmDevnetSpecFile cid = object [
    "config" .= object [
      "chainId" .= (1789 + cid - 20),
      "daoForkSupport" .= True,
      "terminalTotalDifficultyPassed" .= True,
      "terminalTotalDifficulty" .= i 0,
      "daoForkBlock" .= i 0,
      "homesteadBlock" .= i 0,
      "eip150Block" .= i 0,
      "eip155Block" .= i 0,
      "eip158Block" .= i 0,
      "byzantiumBlock" .= i 0,
      "constantinopleBlock" .= i 0,
      "petersburgBlock" .= i 0,
      "istanbulBlock" .= i 0,
      "muirGlacierBlock" .= i 0,
      "berlinBlock" .= i 0,
      "londonBlock" .= i 0,
      "arrowGlacierBlock" .= i 0,
      "graphGlacierBlock" .= i 0,
      "mergeForkBlock" .= i 0,
      "mergeNetsplitBlock" .= i 0,
      "shanghaiTime" .= i 0,
      "cancunTime" .= i 0,
      "pragueTime" .= i 0
    ],
    "timestamp".= t "0x6490fdd2",
    "extraData".= t "0x",
    "gasLimit".= t "0x1c9c380",
    "alloc".= object [
      "0x9b02c3e2df42533e0fd166798b5a616f59dbd2cc".= object [
        "balance".= t "0x0",
        "code".= t "0x6080604052348015600f57600080fd5b506004361060285760003560e01c8063973e55d414602d575b600080fd5b600054603c9063ffffffff1681565b60405163ffffffff909116815260200160405180910390f3fea2646970667358221220b716cf70992d0b5a77124b3da9b37629f5625bf265c121cfb76f9714f249119b64736f6c634300081c0033",
        "storage".= object [
          "0x0000000000000000000000000000000000000000000000000000000000000000" .= (printf "0x%064x" cid :: String)
        ]
      ],
      -- TODO: add native-x-chain system contract
      "0x8849BAbdDcfC1327Ad199877861B577cEBd8A7b6".= object [
        "balance" .= t "0xd3c21bcecceda1000000"
      ],
      "0xFB8Fb7f9bdc8951040a6D195764905138F7462Ed".= object [
        "balance" .= t "0xd3c21bcecceda1000000"
      ],
      "0x28f2d8ef4e0fe6B2E945cF5C33a0118a30a62354".= object [
        "balance" .= t "0xd3c21bcecceda1000000"
      ],
      "0xa24a79678c9fffEF3E9A1f3cb7e51f88F173B3D5".= object [
        "balance" .= t "0xd3c21bcecceda1000000"
      ],
      "0x47fAE86F6416e6115a80635238AFd2F18D69926B".= object [
        "balance" .= t "0xd3c21bcecceda1000000"
      ],
      "0x87466A8266b9DFB3Dc9180a9c43946c4AB2c2cb2".= object [
        "balance" .= t "0xd3c21bcecceda1000000"
      ],
      "0xA310Df9740eb6CC2F5E41C59C87e339142834eA4".= object [
        "balance" .= t "0xd3c21bcecceda1000000"
      ],
      "0xD4EECE51cf451b60F59b271c5a748A8a9F16bC01".= object [
        "balance" .= t "0xd3c21bcecceda1000000"
      ],
      "0xE08643a1C4786b573d739625FD268732dBB3d033".= object [
        "balance" .= t "0xd3c21bcecceda1000000"
      ],
      "0x33018A42499f10B54d9dBCeBB71831C805D64cE3".= object [
        "balance" .= t "0xd3c21bcecceda1000000"
      ],
      "0xa3659D39C901d5985450eE18a63B5b0811fDa521".= object [
        "balance" .= t "0xd3c21bcecceda1000000"
      ],
      "0x7e99c2f1731D3750b74A2a0623C1F1DcB8cCa45e".= object [
        "balance" .= t "0xd3c21bcecceda1000000"
      ],
      "0xFd70Bef78778Ce8554e79D97521b69183960C574".= object [
        "balance" .= t "0xd3c21bcecceda1000000"
      ],
      "0xEE2722c39db6014Eacc5FBe43601136825b00977".= object [
        "balance" .= t "0xd3c21bcecceda1000000"
      ],
      "0xeDD5a9185F9F1C04a011117ad61564415057bf8F".= object [
        "balance" .= t "0xd3c21bcecceda1000000"
      ],
      "0x99b832eb3F76ac3277b00beADC1e487C594ffb4c".= object [
        "balance" .= t "0xd3c21bcecceda1000000"
      ],
      "0xda1380825f827C6Ea92DFB547EF0a341Cbe21d77".= object [
        "balance" .= t "0xd3c21bcecceda1000000"
      ],
      "0xc201d4A5E6De676938533A0997802634E859e78b".= object [
        "balance" .= t "0xd3c21bcecceda1000000"
      ],
      "0x03e95Af0fC4971EdCa12E6d2d1540c28314d15d5".= object [
        "balance" .= t "0xd3c21bcecceda1000000"
      ],
      "0x3492DA004098d728201fD82657f1207a6E5426bd".= object [
        "balance" .= t "0xd3c21bcecceda1000000"
      ]
    ],
    "number" .= t "0x0",
    "nonce" .= t "0x0",
    "difficulty" .= t "0x0",
    "mixHash" .= t "0x0000000000000000000000000000000000000000000000000000000000000000",
    "coinbase" .= t "0x0000000000000000000000000000000000000000"
    ]
  where
    t :: T.Text -> T.Text
    t = id

    i :: Natural -> Natural
    i = id

-- -------------------------------------------------------------------------- --
-- Spec File For EVM Testnet

-- | Used with the public Kadena EVM testnet. This is a temporary feature
-- testnet in preparation for the launch of EVM chains on the Kadena mainet and
-- the regular permanent Kadena testnet.
--
-- The network is expected to be decommissioned after EVM chains have been
-- launched on the Kadena mainnet.
--
-- Funds on the network have no economic value.
--
-- The keys for the genesis allocations are held by the Kadena team.
--
-- The EVM chains are available at block height 0.
--
evmTestnetSpecFile
    :: Natural
        -- ^ numeric chainweb chain id
    -> Value
evmTestnetSpecFile cid = object [
    "config" .= object [
      "chainId" .= (1789 + cid - 20),
      "daoForkSupport" .= True,
      "terminalTotalDifficultyPassed" .= True,
      "terminalTotalDifficulty" .= i 0,
      "daoForkBlock" .= i 0,
      "homesteadBlock" .= i 0,
      "eip150Block" .= i 0,
      "eip155Block" .= i 0,
      "eip158Block" .= i 0,
      "byzantiumBlock" .= i 0,
      "constantinopleBlock" .= i 0,
      "petersburgBlock" .= i 0,
      "istanbulBlock" .= i 0,
      "muirGlacierBlock" .= i 0,
      "berlinBlock" .= i 0,
      "londonBlock" .= i 0,
      "arrowGlacierBlock" .= i 0,
      "graphGlacierBlock" .= i 0,
      "mergeForkBlock" .= i 0,
      "mergeNetsplitBlock" .= i 0,
      "shanghaiTime" .= i 0,
      "cancunTime" .= i 0,
      "pragueTime" .= i 0
    ],
    "timestamp".= t "0x6490fdd2",
    "extraData".= t "0x",
    "gasLimit".= t "0x1c9c380",
    "alloc".= object [
      "0x9b02c3e2df42533e0fd166798b5a616f59dbd2cc".= object [
        "balance".= t "0x0",
        "code".= t "0x6080604052348015600f57600080fd5b506004361060285760003560e01c8063973e55d414602d575b600080fd5b600054603c9063ffffffff1681565b60405163ffffffff909116815260200160405180910390f3fea2646970667358221220b716cf70992d0b5a77124b3da9b37629f5625bf265c121cfb76f9714f249119b64736f6c634300081c0033",
        "storage".= object [
          "0x0000000000000000000000000000000000000000000000000000000000000000" .= (printf "0x%064x" cid :: String)
        ]
      ]
      -- TODO: Native-X-Chain System contract
      -- TODO: funding of faucet
      -- TODO: other allocations.
    ],

    "number" .= t "0x0",
    "nonce" .= t "0x0",
    "difficulty" .= t "0x0",
    "mixHash" .= t "0x0000000000000000000000000000000000000000000000000000000000000000",
    "coinbase" .= t "0x0000000000000000000000000000000000000000"
    ]
  where
    t :: T.Text -> T.Text
    t = id

    i :: Natural -> Natural
    i = id

-- -------------------------------------------------------------------------- --
-- Spec File For Kadena Mainnet

-- | Used with the public Kadena Mainnet.
--
-- Allocations are funded out of the platform share of the Kadena mainnet.
-- The keys for the Allocations are not publicly known.
--
-- The EVM chains are launched at block height TODO.
--
-- TODO
--
mainnetSpecFile
    :: Natural
        -- ^ numeric chainweb chain id
    -> Value
mainnetSpecFile cid = object [
    "config" .= object [
      "chainId" .= (3789 + cid - 20),
      "daoForkSupport" .= True,
      "terminalTotalDifficultyPassed" .= True,
      "terminalTotalDifficulty" .= i 0,
      "daoForkBlock" .= i 0,
      "homesteadBlock" .= i 0,
      "eip150Block" .= i 0,
      "eip155Block" .= i 0,
      "eip158Block" .= i 0,
      "byzantiumBlock" .= i 0,
      "constantinopleBlock" .= i 0,
      "petersburgBlock" .= i 0,
      "istanbulBlock" .= i 0,
      "muirGlacierBlock" .= i 0,
      "berlinBlock" .= i 0,
      "londonBlock" .= i 0,
      "arrowGlacierBlock" .= i 0,
      "graphGlacierBlock" .= i 0,
      "mergeForkBlock" .= i 0,
      "mergeNetsplitBlock" .= i 0,
      "shanghaiTime" .= i 0,
      "cancunTime" .= i 0,
      "pragueTime" .= i 0
    ],
    "timestamp".= t "0x6490fdd2",
    "extraData".= t "0x",
    "gasLimit".= t "0x1c9c380",
    "alloc".= object [
      "0x9b02c3e2df42533e0fd166798b5a616f59dbd2cc".= object [
        "balance".= t "0x0",
        "code".= t "0x6080604052348015600f57600080fd5b506004361060285760003560e01c8063973e55d414602d575b600080fd5b600054603c9063ffffffff1681565b60405163ffffffff909116815260200160405180910390f3fea2646970667358221220b716cf70992d0b5a77124b3da9b37629f5625bf265c121cfb76f9714f249119b64736f6c634300081c0033",
        "storage".= object [
          "0x0000000000000000000000000000000000000000000000000000000000000000" .= (printf "0x%064x" cid :: String)
        ]
      ],
      error "mainnetSpecFile: the EVM genesis allocations for mainnet are TBD"
      -- TODO: Native-X-Chain System contract
      -- TODO: other allocations.
    ],

    "number" .= error @_ @() "mainnetSpecFile: the initial block height is TBD", -- t "0x0",
    "nonce" .= t "0x0",
    "difficulty" .= t "0x0",
    "mixHash" .= t "0x0000000000000000000000000000000000000000000000000000000000000000",
    "coinbase" .= t "0x0000000000000000000000000000000000000000"
    ]
  where
    t :: T.Text -> T.Text
    t = id

    i :: Natural -> Natural
    i = id


-- -------------------------------------------------------------------------- --
-- Spec File For Kadena Testnet

-- | Used with the public Kadena testnet. This is a permament testnet that
-- has the same features and properties of the Kadena mainnet. It has the
-- purpose to facilitate testing of services and applications under the same
-- conditions as on the Kadena mainnet.
--
-- Funds on the network have no economic value.
--
-- The keys for the genesis allocations are held by the Kadena team.
--
-- The EVM chains are launched at block height TODO.
--
-- TODO
--
testnetSpecFile
    :: Natural
        -- ^ numeric chainweb chain id
    -> Value
testnetSpecFile cid = object [
    "config" .= object [
      "chainId" .= (2789 + cid - 20),
      "daoForkSupport" .= True,
      "terminalTotalDifficultyPassed" .= True,
      "terminalTotalDifficulty" .= i 0,
      "daoForkBlock" .= i 0,
      "homesteadBlock" .= i 0,
      "eip150Block" .= i 0,
      "eip155Block" .= i 0,
      "eip158Block" .= i 0,
      "byzantiumBlock" .= i 0,
      "constantinopleBlock" .= i 0,
      "petersburgBlock" .= i 0,
      "istanbulBlock" .= i 0,
      "muirGlacierBlock" .= i 0,
      "berlinBlock" .= i 0,
      "londonBlock" .= i 0,
      "arrowGlacierBlock" .= i 0,
      "graphGlacierBlock" .= i 0,
      "mergeForkBlock" .= i 0,
      "mergeNetsplitBlock" .= i 0,
      "shanghaiTime" .= i 0,
      "cancunTime" .= i 0,
      "pragueTime" .= i 0
    ],
    "timestamp".= t "0x6490fdd2",
    "extraData".= t "0x",
    "gasLimit".= t "0x1c9c380",
    "alloc".= object [
      "0x9b02c3e2df42533e0fd166798b5a616f59dbd2cc".= object [
        "balance".= t "0x0",
        "code".= t "0x6080604052348015600f57600080fd5b506004361060285760003560e01c8063973e55d414602d575b600080fd5b600054603c9063ffffffff1681565b60405163ffffffff909116815260200160405180910390f3fea2646970667358221220b716cf70992d0b5a77124b3da9b37629f5625bf265c121cfb76f9714f249119b64736f6c634300081c0033",
        "storage".= object [
          "0x0000000000000000000000000000000000000000000000000000000000000000" .= (printf "0x%064x" cid :: String)
        ]
      ],
      error "mainnetSpecFile: the EVM genesis allocations for mainnet are TBD"
      -- TODO: Native-X-Chain System contract
      -- TODO: other allocations.
    ],

    "number" .= error @_ @() "mainnetSpecFile: the initial block height is TBD", -- t "0x0",
    "nonce" .= t "0x0",
    "difficulty" .= t "0x0",
    "mixHash" .= t "0x0000000000000000000000000000000000000000000000000000000000000000",
    "coinbase" .= t "0x0000000000000000000000000000000000000000"
    ]
  where
    t :: T.Text -> T.Text
    t = id

    i :: Natural -> Natural
    i = id
