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
            let cids = [20..24]
            return ("mainnet", cids, mainnetSpecFile)
        ["testnet"] -> do
            let cids = [20..24]
            return ("testnet", cids, testnetSpecFile)
        ["evm-testnet"] -> do
            let cids = [20..24]
            return ("evm-testnet", cids, evmTestnetSpecFile)
        ["evm-development"] -> do
            let cids = [20..24]
            return ("evm-development", cids, evmDevnetSpecFile 20)
        ["evm-development-singleton"] -> do
            let cids = [0]
            return ("evm-development-singleton", cids, evmDevnetSpecFile 0)
        ["evm-development-pair"] -> do
            let cids = [1]
            return ("evm-development-pair", cids, evmDevnetSpecFile 1)
        _ -> error "Invalid argument for the chainweb version provided. The version must be one of: 'mainnet', 'testnet', 'evm-testnet', or 'evm-development'."

    let specFileDir = "./chain-specs/" <> n
    createDirectoryIfMissing True specFileDir
    hdrs <- forM cids $ \cid -> do
        let specFileName = specFileDir <> "/chain-spec-" <> show cid <> ".json"
        encodeFile specFileName $ spec cid
        hdr <- queryNode cid specFileName
        return (cid, hdr)

    let payloadFileName = specFileDir <> "/payloads.json"
    encodeFile payloadFileName $
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
-- Spec file settings

-- |
--
-- System contract addresses:
--
-- * Chainweb-chainId system contract:
--   0x9b02c3e2dF42533e0FD166798B5A616f59DBd2cc
--   Keccak256("/Chainweb/Chain/Id/")
--
-- * Native X-Chan redeem system contract:
--   0x49eed2ac33f09e931bd660f0168417b9614485b6
--   Keccack256("/Chainweb/XChan/Redeem/")
--
-- * ERC-20 x-chain SPV Precompile address from KIP-34 (EthDenver 2025 demo):
--   48c3b4d2757447601776837b6a85f31ef88a87bf
--   Keccak256("/Chainweb/KIP-34/VERIFY/SVP/")
--
baseSpecFile
    :: Natural
        -- ^ Ethereum network id offset
    -> Natural
        -- ^ offset for the chainweb chain id
    -> Natural
        -- ^ numeric chainweb chain id
    -> Natural
        -- ^ unix timestamp of the genesis block in textual hex encoding
    -> [(Key, Value)]
        -- ^ Allocations
    -> Value
baseSpecFile netId offset cid genesisTime allocs = object
    [ "config" .= object
        [ "chainId" .= (netId + cid - offset)
        , "daoForkSupport" .= True
        , "terminalTotalDifficultyPassed" .= True
        , "terminalTotalDifficulty" .= i 0
        , "daoForkBlock" .= i 0
        , "homesteadBlock" .= i 0
        , "eip150Block" .= i 0
        , "eip155Block" .= i 0
        , "eip158Block" .= i 0
        , "byzantiumBlock" .= i 0
        , "constantinopleBlock" .= i 0
        , "petersburgBlock" .= i 0
        , "istanbulBlock" .= i 0
        , "muirGlacierBlock" .= i 0
        , "berlinBlock" .= i 0
        , "londonBlock" .= i 0
        , "arrowGlacierBlock" .= i 0
        , "graphGlacierBlock" .= i 0
        , "mergeForkBlock" .= i 0
        , "mergeNetsplitBlock" .= i 0
        , "shanghaiTime" .= i 0
        , "cancunTime" .= i 0
        , "pragueTime" .= i 0
        , "blobSchedule" .= object
            [ "cancun" .= object
                [ "target" .= i 0
                , "max" .= i 0
                , "baseFeeUpdateFraction" .= i 3338477
                ]
            , "prague" .= object
                [ "target" .= i 0
                , "max" .= i 0
                , "baseFeeUpdateFraction" .= i 5007716
                ]
            ]
        ]
    , "timestamp" .= printf @(Natural -> String) "0x%x" genesisTime
    , "extraData" .= t "0x"
    , "gasLimit" .= t "0x1c9c380"
    , "alloc" .= object
        ( chainwebChainIdAlloc
        : xChanRedeemAlloc
        : eip4788Alloc
        : eip2935Alloc
        : allocs
        )
    , "number" .= t "0x0"
    , "nonce" .= t "0x0"
    , "difficulty" .= t "0x0"
    , "mixHash" .= zero32 @T.Text
    , "coinbase" .= zero20 @T.Text
    ]
  where
    i :: Natural -> Natural
    i = id

    -- Chainweb System Contracts: Keccak256("/Chainweb/Chain/Id/")
    --
    chainwebChainIdAlloc = "0x9b02c3e2df42533e0fd166798b5a616f59dbd2cc" .= object
        [ "balance" .= t "0x0"
        , "code" .= t "0x5f545f526004601cf3"
        , "storage" .= object [ zero32 .= (printf "0x%064x" cid :: String) ]
        ]

    -- Native X-Chan redeem system contract: Keccack256("/Chainweb/XChan/Redeem/")
    -- TODO: cleanup and optimize the code
    xChanRedeemAlloc = "0x49eed2ac33f09e931bd660f0168417b9614485b6" .= object
        [ "balance" .= t "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
        , "code" .= t "0x346101535773ad9923c37370bcbcf00ed194506d8950848956965f9060209160c060e091610140946101609061018091609a9660e3360361014c575f359860203560f01c9360223560ea1c9860263560601c9a603a3599605a359b607a359a5f60db3560c01c9a03610145576004815f80739b02c3e2df42533e0fd166798b5a616f59dbd2cc5afa1561013e5751036101375760408593602060018580896080985f869c372086528181601f8601370191013760015afa1561013057510361012957602083918193720f3df6d732807ef1319fb7b8bb8522d0beac029082525afa1561012257510361011b5781548091038411610114575f84819482948284950190555af11561010d575f80f35b600a610157565b6009610157565b6008610157565b6007610157565b6006610157565b6005610157565b6004610157565b6003610157565b6002610157565b6001610157565b5f80fd5b5f5260205ffd"
        ]

    -- Official Ethreum System Contracts:

    -- EIP-4788 BeaconRoot Oracle. Included in Cancun fork
    eip4788Alloc = "0x000F3df6D732807Ef1319fB7B8bB8522d0Beac02" .= object
        [ "balance" .= t "0x0"
        , "code" .= t "0x3373fffffffffffffffffffffffffffffffffffffffe14604d57602036146024575f5ffd5b5f35801560495762001fff810690815414603c575f5ffd5b62001fff01545f5260205ff35b5f5ffd5b62001fff42064281555f359062001fff015500"
        ]

    -- EIP-2935 Historical Block Hashes. Included in the Prague fork
    eip2935Alloc = "0x0000F90827F1C53a10cb7A02335B175320002935" .= object
        [ "balance" .= t "0x0"
        , "code" .= t "0x3373fffffffffffffffffffffffffffffffffffffffe14604657602036036042575f35600143038111604257611fff81430311604257611fff9006545f5260205ff35b5f5ffd5b5f35611fff60014303065500"
        ]

    -- EIP-7002 EL Triggered Withdrawal Requests. Included in Prague fork
    -- Not available on Kadena EVM networks

    -- EIP-7251 EL triggered Consolidations. Included in Prague fork
    -- Not available on Kadena EVM networks

zero32 :: IsString s => s
zero32 = "0x0000000000000000000000000000000000000000000000000000000000000000"

zero20 :: IsString s => s
zero20 = "0x0000000000000000000000000000000000000000"

t :: T.Text -> T.Text
t = id

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
        -- ^ offset for the chain id
    -> Natural
        -- numeric chainweb chain id
    -> Value
evmDevnetSpecFile offset cid = baseSpecFile 1789 offset cid 0x684c5d2a
    [ "0x8849BAbdDcfC1327Ad199877861B577cEBd8A7b6" .= object
        [ "balance" .= t "0xd3c21bcecceda1000000" ]
    , "0xFB8Fb7f9bdc8951040a6D195764905138F7462Ed" .= object
        [ "balance" .= t "0xd3c21bcecceda1000000" ]
    , "0x28f2d8ef4e0fe6B2E945cF5C33a0118a30a62354" .= object
        [ "balance" .= t "0xd3c21bcecceda1000000" ]
    , "0xa24a79678c9fffEF3E9A1f3cb7e51f88F173B3D5" .= object
        [ "balance" .= t "0xd3c21bcecceda1000000" ]
    , "0x47fAE86F6416e6115a80635238AFd2F18D69926B" .= object
        [ "balance" .= t "0xd3c21bcecceda1000000" ]
    , "0x87466A8266b9DFB3Dc9180a9c43946c4AB2c2cb2" .= object
        [ "balance" .= t "0xd3c21bcecceda1000000" ]
    , "0xA310Df9740eb6CC2F5E41C59C87e339142834eA4" .= object
        [ "balance" .= t "0xd3c21bcecceda1000000" ]
    , "0xD4EECE51cf451b60F59b271c5a748A8a9F16bC01" .= object
        [ "balance" .= t "0xd3c21bcecceda1000000" ]
    , "0xE08643a1C4786b573d739625FD268732dBB3d033" .= object
        [ "balance" .= t "0xd3c21bcecceda1000000" ]
    , "0x33018A42499f10B54d9dBCeBB71831C805D64cE3" .= object
        [ "balance" .= t "0xd3c21bcecceda1000000" ]
    , "0xa3659D39C901d5985450eE18a63B5b0811fDa521" .= object
        [ "balance" .= t "0xd3c21bcecceda1000000" ]
    , "0x7e99c2f1731D3750b74A2a0623C1F1DcB8cCa45e" .= object
        [ "balance" .= t "0xd3c21bcecceda1000000" ]
    , "0xFd70Bef78778Ce8554e79D97521b69183960C574" .= object
        [ "balance" .= t "0xd3c21bcecceda1000000" ]
    , "0xEE2722c39db6014Eacc5FBe43601136825b00977" .= object
        [ "balance" .= t "0xd3c21bcecceda1000000" ]
    , "0xeDD5a9185F9F1C04a011117ad61564415057bf8F" .= object
        [ "balance" .= t "0xd3c21bcecceda1000000" ]
    , "0x99b832eb3F76ac3277b00beADC1e487C594ffb4c" .= object
        [ "balance" .= t "0xd3c21bcecceda1000000" ]
    , "0xda1380825f827C6Ea92DFB547EF0a341Cbe21d77" .= object
        [ "balance" .= t "0xd3c21bcecceda1000000" ]
    , "0xc201d4A5E6De676938533A0997802634E859e78b" .= object
        [ "balance" .= t "0xd3c21bcecceda1000000" ]
    , "0x03e95Af0fC4971EdCa12E6d2d1540c28314d15d5" .= object
        [ "balance" .= t "0xd3c21bcecceda1000000" ]
    , "0x3492DA004098d728201fD82657f1207a6E5426bd" .= object
        [ "balance" .= t "0xd3c21bcecceda1000000" ]

    ]

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
        -- numeric chainweb chain id
    -> Value
evmTestnetSpecFile cid = baseSpecFile 5920 20 cid 0x684c5d2a
    -- faucet deployer address that corresponds to the DEPLOYER_PRIVATE_KEY
    [ "0x9440d8ff19D278F401f49080BEfdDEFbE54F0eF2" .= object
        [ "balance" .= t "0x422ca8b0a00a425000000" ]
    -- is the faucet wallet address that correspondesds to the FAUCET_PRIVATE_KEY
    , "0xE482e4F590D4155B51F4Fc21d64823f4d7854397" .= object
        [ "balance" .= t "0x422ca8b0a00a425000000" ]
    -- additional platform funds that are separate from the faucet accounts
    , "0xeC1B36992C3c7d0f7AbB8EDA43EEbC9A418c0A1e" .= object
        [ "balance" .= t "0x422ca8b0a00a425000000" ]
    ]

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
-- TODO: update genesis timestamp and block number
--
testnetSpecFile
    :: Natural
        -- numeric chainweb chain id
    -> Value
testnetSpecFile cid = baseSpecFile 5910 20 cid 0x684c5d2a
    [
      error "mainnetSpecFile: the EVM genesis allocations for mainnet are TBD"
      -- TODO: Native-X-Chain System contract
      -- TODO: other allocations.
    ]

-- -------------------------------------------------------------------------- --
-- Spec File For Kadena Mainnet

-- | Used with the public Kadena Mainnet.
--
-- Allocations are funded out of the platform share of the Kadena mainnet.
-- The keys for the Allocations are not publicly known.
--
-- The EVM chains are launched at block height TODO.
--
-- TODO: update genesis timestamp and block number
--
mainnetSpecFile
    :: Natural
        -- numeric chainweb chain id
    -> Value
mainnetSpecFile cid = baseSpecFile 5900 20 cid 0x684c5d2a
    [
      error "mainnetSpecFile: the EVM genesis allocations for mainnet are TBD"
      -- TODO: Native-X-Chain System contract
      -- TODO: other allocations.
    ]

