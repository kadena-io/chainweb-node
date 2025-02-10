{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Chainweb.PayloadProvider.EVM.Genesis
-- Copyright: Copyright © 2025 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.PayloadProvider.EVM.Genesis
( genesisBlocks
) where
import Chainweb.Version
import Chainweb.PayloadProvider.EVM.Header
import Chainweb.Version.EvmDevelopment
import Chainweb.Utils
import Chainweb.PayloadProvider.EVM.Utils (decodeRlpM)

-- -------------------------------------------------------------------------- --
-- Genesis Blocks
--
-- EVM Development headers for chain-0 and chain-1.
--
-- NOTE that the header value does not depend on the chainId/netId.
--
-- {
--   "hash": "0x557c3fc4b87c8bce45002e8fe4db07546324caec6459cb67c64389110c9fd942",
--   "parentHash": "0x0000000000000000000000000000000000000000000000000000000000000000",
--   "sha3Uncles": "0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347",
--   "miner": "0x0000000000000000000000000000000000000000",
--   "stateRoot": "0x7d2b86c493412a586f03e4aee1ab94837c6a8fff21ff7c6045dc1330d5b0d49e",
--   "transactionsRoot": "0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
--   "receiptsRoot": "0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
--   "logsBloom": "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
--   "difficulty": "0x0",
--   "number": "0x0",
--   "gasLimit": "0x1c9c380",
--   "gasUsed": "0x0",
--   "timestamp": "0x6490fdd2",
--   "totalDifficulty": "0x0",
--   "extraData": "0x",
--   "mixHash": "0x0000000000000000000000000000000000000000000000000000000000000000",
--   "nonce": "0x0000000000000000",
--   "baseFeePerGas": "0x3b9aca00",
--   "withdrawalsRoot": "0x56e81f171bcc55a6ff8345e692c0f86e5b48e01b996cadc001622fb5e363b421",
--   "blobGasUsed": "0x0",
--   "excessBlobGas": "0x0",
--   "parentBeaconBlockRoot": "0x0000000000000000000000000000000000000000000000000000000000000000",
--   "uncles": [],
--   "transactions": [],
--   "size": "0x247",
--   "withdrawals": []
-- }
--
-- block payload hash: -EffZgInrtrokWVn5iqO2fuTbtG2-noDdhXr0pHQA4E

-- | Genesis Headers for the EVM Payload Provider
--
-- NOTE: These headers must match the block payload hashes in the respective
-- Chainweb.Version.* modules.
--
-- How to get the headers:
--
-- 1. Run the EVM with the chain specification for the network:
--
--    @
--    docker compose up -d chainweb-evm-chain0
--    @
--
-- 2. Query the EVM genesis header and compute block payload hash and header:
--
--    @
--    cabal run cwtools:exe:evm-genesis -- http://localhost:8545
--    @
--
genesisBlocks
    :: HasChainwebVersion v
    => HasChainId c
    => v
    -> c
    -> Header
genesisBlocks v c = go (_chainwebVersion v) (_chainId c)
  where
    -- Ethereum NetworkID 1789
    go EvmDevelopment (ChainId 0) = f
        "-QI-oAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoB3MTejex116q4W1Z7bM1BrTEkUblIp0E_ChQv1A1JNHlAAAAAAAAAAAAAAAAAAAAAAAAAAAoKubUszUzclOu3VLZ6Oz-L6Nph9pRr7Ilc6Sy0N9diB6oFboHxcbzFWm_4NF5pLA-G5bSOAbmWytwAFiL7XjY7QhoFboHxcbzFWm_4NF5pLA-G5bSOAbmWytwAFiL7XjY7QhuQEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAICAhAHJw4CAhGSQ_dKAoAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAiAAAAAAAAAAAhDuaygCgVugfFxvMVab_g0XmksD4bltI4BuZbK3AAWIvteNjtCGAgKAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
    -- Ethereum NetworkID 1790
    go EvmDevelopment (ChainId 1) = f
        "-QI-oAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAoB3MTejex116q4W1Z7bM1BrTEkUblIp0E_ChQv1A1JNHlAAAAAAAAAAAAAAAAAAAAAAAAAAAoKtIkoG2zy4Z3rQ8TVs8INePmK-1cYAD8aBvkf-57uIVoFboHxcbzFWm_4NF5pLA-G5bSOAbmWytwAFiL7XjY7QhoFboHxcbzFWm_4NF5pLA-G5bSOAbmWytwAFiL7XjY7QhuQEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAICAhAHJw4CAhGSQ_dKAoAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAiAAAAAAAAAAAhDuaygCgVugfFxvMVab_g0XmksD4bltI4BuZbK3AAWIvteNjtCGAgKAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
    go _ _ = error "requested genesis block for unsupported chain"

    f t = case decodeB64UrlNoPaddingText t >>= decodeRlpM of
        Left e -> error $ "Chainweb.PayloadProvider.EVM.genesisBlocks: invalid genesis block: " <> sshow e
        Right x -> x

