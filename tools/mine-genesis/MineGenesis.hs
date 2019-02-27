{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module: MineGenesis
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- Generate a legal genesis block.
--
module Main where

import Control.Lens (over)

import Data.Aeson.Encode.Pretty
import Data.ByteString.Char8 as B8
import Data.ByteString.Lazy.Char8 as BL8
import Data.Bytes.Put
import Data.Generics.Wrapped (_Unwrapped)
import Data.Int (Int64)
import Data.Word (Word32)
import qualified Data.Yaml as Yaml

import Options.Generic

-- internal modules

import Chainweb.BlockHeader
import Chainweb.ChainId (ChainId, testChainId)
import Chainweb.Difficulty (checkTarget)
import Chainweb.Time (Time(..), TimeSpan(..), getCurrentTimeIntegral)
import Chainweb.Version (ChainwebVersion(..), chainwebVersionFromText)

---

data Format = Json | Yaml | Binary
    deriving (Generic, Read)

instance ParseField Format

data Env w = Env
    { version :: w ::: Text     <?> "The ChainwebVersion to use."
    , chain :: w ::: Word32     <?> "The ChainId to produce a genesis for."
    , time :: w ::: Maybe Int64 <?> "Genesis Block Time, in microseconds since the Epoch. Otherwise, uses the current time."
    , format :: w ::: Maybe Format <?> "Output format Json|Yaml|Binary, default is Json"
    } deriving (Generic)

instance ParseRecord (Env Wrapped)

main :: IO ()
main = do
    Env v0 c t f <- unwrapRecord "mine-genesis"
    ct <- BlockCreationTime <$> maybe getCurrentTimeIntegral (pure . Time . TimeSpan) t
    v <- chainwebVersionFromText v0

    case f of
        Just Binary -> BL8.putStr $ runPutL
            $ encodeBlockHeader $ mineGenesis v (testChainId c) ct (Nonce 0)
        Just Yaml -> B8.putStrLn $ Yaml.encode
            $ ObjectEncoded $ mineGenesis v (testChainId c) ct (Nonce 0)
        _ -> BL8.putStrLn $ encodePretty
            $ ObjectEncoded $ mineGenesis v (testChainId c) ct (Nonce 0)

mineGenesis
    :: ChainwebVersion
    -> ChainId
    -> BlockCreationTime
    -> Nonce
    -> BlockHeader
mineGenesis v p ct n
    | checkTarget (_blockTarget gh) (_blockPow gh) = gh
    | otherwise = mineGenesis v p ct (over _Unwrapped succ n)
  where
    gh = genesisBlockHeader' v p ct n
