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
import Data.Bytes.Put
import Data.ByteString.Lazy.Char8 as BL8
import Data.Generics.Wrapped (_Unwrapped)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as TL
import Data.Word (Word32)
import qualified Data.Yaml as Yaml

import Options.Generic

import Text.Pretty.Simple (pShowNoColor)

-- internal modules

import Chainweb.BlockHeader
import Chainweb.ChainId (ChainId, testChainId)
import Chainweb.Difficulty (checkTarget)
import Chainweb.Time (Time(..), TimeSpan(..))
import Chainweb.Version (ChainwebVersion(..), chainwebVersionFromText)

---

data Format = Json | Yaml | Binary | Show
    deriving (Generic, Read)

instance ParseField Format

data Env w = Env
    { version :: w ::: Text     <?> "The ChainwebVersion to use."
    , chain :: w ::: Word32     <?> "The ChainId to produce a genesis for."
    , time :: w ::: Maybe Int64 <?> "Genesis Block Time, in microseconds since the Epoch. Default is the Genesis Time of the given ChainwebVersion."
    , format :: w ::: Maybe Format <?> "Output format Json|Yaml|Binary|Show, default is Show"
    } deriving (Generic)

instance ParseRecord (Env Wrapped)

main :: IO ()
main = do
    Env v0 c t f0 <- unwrapRecord "mine-genesis"
    v <- chainwebVersionFromText v0
    let cid = testChainId c
        ct = maybe (genesisTime v cid) (BlockCreationTime . Time . TimeSpan) t
        f = fromMaybe Show f0
    BL8.putStrLn . encodeBlock f $ mineGenesis v cid ct (Nonce 0)

encodeBlock :: Format -> BlockHeader -> BL8.ByteString
encodeBlock Binary bh = runPutL $ encodeBlockHeader bh
encodeBlock Yaml bh = BL8.fromStrict . Yaml.encode $ ObjectEncoded bh
encodeBlock Json bh = encodePretty $ ObjectEncoded bh
encodeBlock Show bh = BL8.pack . TL.unpack $ pShowNoColor bh

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
