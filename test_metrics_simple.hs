#!/usr/bin/env cabal
{- cabal:
build-depends: base, http-types, wai, warp, text, bytestring
-}

-- Simple test to verify metrics endpoint compiles and can serve
module Main where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8

-- Simplified metrics response
metricsApp :: Application
metricsApp req respond =
    case pathInfo req of
        ["metrics"] -> respond $ responseLBS
            status200
            [("Content-Type", "text/plain; version=0.0.4")]
            sampleMetrics
        _ -> respond $ responseLBS
            status404
            []
            "Not Found"

sampleMetrics :: L8.ByteString
sampleMetrics = L8.unlines
    [ "# HELP chainweb_block_height Current block height per chain"
    , "# TYPE chainweb_block_height gauge"
    , "chainweb_block_height{chain_id=\"0\"} 1000"
    , "chainweb_block_height{chain_id=\"1\"} 999"
    , ""
    , "# HELP chainweb_cut_height Current cut height"
    , "# TYPE chainweb_cut_height gauge"
    , "chainweb_cut_height 50000"
    , ""
    , "# HELP chainweb_mempool_size Current mempool size"
    , "# TYPE chainweb_mempool_size gauge"
    , "chainweb_mempool_size{chain_id=\"0\"} 42"
    ]

main :: IO ()
main = do
    putStrLn "Starting test metrics server on http://localhost:3000/metrics"
    run 3000 metricsApp