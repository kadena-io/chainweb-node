{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

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
import Data.String
import Data.Text.IO qualified as T
import Ethereum.Misc qualified as E
import Ethereum.RLP qualified as E
import Network.HTTP.Client qualified as HTTP
import Network.URI
import System.Environment

main :: IO ()
main = do
    -- read uri form command line
    uri <- getArgs >>= \case
        [str] -> fromText (fromString str)
        l -> error $ "unexpected CLI arguments " <> show l

    -- query header
    ctx <- mkRpcCtx uri
    hdr <- getBlockAtNumber ctx 0

    -- print block payload hash
    T.putStrLn $ encodeToText $ E._hdrPayloadHash hdr
    -- encode header to base64
    T.putStrLn $ encodeB64UrlNoPaddingText $ E.putRlpByteString hdr

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

