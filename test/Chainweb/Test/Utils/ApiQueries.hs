{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Chainweb.Test.Utils.ApiQueries
-- Copyright: Copyright © 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.Test.Utils.ApiQueries
( mkMgr
, runQuery
, getHeaderByHash
, getHeaderByHeight
) where

import Chainweb.BlockHash
import Chainweb.TreeDB hiding (lookup)

import Control.Lens hiding ((.=))
import Control.Monad

import qualified Data.HashSet as HS

import GHC.Stack

import qualified Network.Connection as HTTP
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP

import qualified Servant.Client as HTTP

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB.RestAPI.Client
import Chainweb.BlockHeight
import Chainweb.Cut.CutHashes
import Chainweb.CutDB.RestAPI.Client
import Chainweb.Utils
import Chainweb.Utils.Paging
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Endpoints

endpoint :: HasCallStack => HTTP.Manager -> ChainwebVersion -> HTTP.ClientEnv
endpoint mgr Mainnet01 = HTTP.mkClientEnv mgr $ HTTP.BaseUrl HTTP.Https "us-e1.chainweb.com" 443 ""
endpoint mgr Testnet04 = HTTP.mkClientEnv mgr $ HTTP.BaseUrl HTTP.Https "us1.testnet.chainweb.com" 443 ""
endpoint mgr Development = HTTP.mkClientEnv mgr $ HTTP.BaseUrl HTTP.Https "us1.tn.chainweb.com" 443 ""
endpoint _ x = error $ "endpoint: unsupported chainweb version " <> sshow x

-- -------------------------------------------------------------------------- --
-- Tools

mkMgr :: IO HTTP.Manager
mkMgr = HTTP.newTlsManagerWith $ HTTP.mkManagerSettings
    (HTTP.TLSSettingsSimple True True True)
    Nothing

runQuery
    :: HasCallStack
    => HTTP.Manager
    -> ChainwebVersion
    -> HTTP.ClientM a
    -> IO a
runQuery mgr v q = HTTP.runClientM q (endpoint mgr v) >>= \case
    Left e -> error (show e)
    Right x -> return x

-- -------------------------------------------------------------------------- --
-- API Queries

getHeaderByHash
    :: HasCallStack
    => HTTP.Manager
    -> ChainwebVersion
    -> ChainId
    -> BlockHash
    -> IO BlockHeader
getHeaderByHash mgr v c = runQuery mgr v . headerClient v c

getHeaderByHeight
    :: HasCallStack
    => HTTP.Manager
    -> ChainwebVersion
    -> ChainId
    -> BlockHeight
    -> IO BlockHeader
getHeaderByHeight mgr v cid height = do
    BlockHashWithHeight curHeight curHash <- currentHash mgr v cid
    when (curHeight < height) $
        error $ "getHeaderByHeight: height " <> sshow height <> " is in the future. Current height " <> sshow curHeight
    page <- runQuery mgr v $ query curHash
    when (_pageLimit page < 1) $
        error $ "getHeaderByHeight: no header found for height " <> sshow height <> " on chain " <> sshow cid
    when (_pageLimit page > 1) $
        error $ "getHeaderByHeight: expect just one result but got " <> sshow (_pageLimit page)
    return $ head $ _pageItems page
  where
    query b = branchHeadersClient v cid (Just 1) Nothing
        Nothing
        (Just $ int height)
        (BranchBounds mempty $ HS.singleton (UpperBound b))

currentHash
    :: HasCallStack
    => HTTP.Manager
    -> ChainwebVersion
    -> ChainId
    -> IO BlockHashWithHeight
currentHash mgr v cid = do
    c <- runQuery mgr v $ cutGetClient v
    return $ c ^?! cutHashes . ix cid

