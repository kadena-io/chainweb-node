{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.TreeDB.SyncSession
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.TreeDB.SyncSession
( syncSession
, type BlockHeaderTreeDb
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Lens ((&))
import Control.Monad

import Data.Functor.Of
import Data.Hashable
import qualified Data.HashSet as HS
import qualified Data.Text as T

import GHC.Generics

import Servant.Client

import Streaming
import qualified Streaming.Prelude as S

import System.IO.Unsafe
import System.LogLevel

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.RestAPI.Client
import Chainweb.ChainId
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Utils.Paging
import Chainweb.Version

import Data.LogMessage

import P2P.Session

-- -------------------------------------------------------------------------- --
-- BlockHeaderDb Utils

type BlockHeaderTreeDb db = (TreeDb db, DbEntry db ~ BlockHeader)

-- TODO: add TreeDb instance
--
data ChainClientEnv = ChainClientEnv
    { _envChainwebVersion :: !ChainwebVersion
    , _envChainId :: !ChainId
    , _envClientEnv :: !ClientEnv
    }
    deriving (Generic)

-- | TODO: add retry logic
--
runUnpaged
    :: ClientEnv
    -> Maybe k
    -> Maybe Limit
    -> (Maybe k -> Maybe Limit -> ClientM (Page k a))
    -> Stream (Of a) IO ()
runUnpaged env a b req = go a b
  where
    go k l = lift (runClientThrowM (req k l) env) >>= \page -> do
        let c = (\x -> x - _pageLimit page) <$> l
        S.each (_pageItems page)
        case c of
            Just x | x <= 0 -> return ()
            _ -> maybe (return ()) (\n -> go (Just n) c) (_pageNext page)

runClientThrowM :: ClientM a -> ClientEnv -> IO a
runClientThrowM req = fromEitherM <=< runClientM req

getBranchHeaders
    :: ChainClientEnv
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> Stream (Of (DbEntry BlockHeaderDb)) IO ()
getBranchHeaders (ChainClientEnv v cid env) minr maxr range = runUnpaged env Nothing Nothing $ \k l ->
    branchHeadersClient v cid l k minr maxr range

getLeaves
    :: ChainClientEnv
    -> Maybe MinRank
    -> Maybe MaxRank
    -> Stream (Of (DbKey BlockHeaderDb)) IO ()
getLeaves (ChainClientEnv v cid env) minr maxr = runUnpaged env Nothing Nothing $ \k l ->
    leafHashesClient v cid l k minr maxr

putHeader
    :: ChainClientEnv
    -> DbEntry BlockHeaderDb
    -> IO ()
putHeader (ChainClientEnv v cid env) = void . flip runClientThrowM env
    . headerPutClient v cid

-- -------------------------------------------------------------------------- --
-- Sync

fullSync :: BlockHeaderTreeDb db => db -> LogFunction -> ChainClientEnv -> IO ()
fullSync ldb logFun env = do

    logg Debug "get local leaves"
    lLeaves <- streamToHashSet_ $ leafKeys ldb Nothing Nothing Nothing Nothing

    logg Debug "get remote leaves"
    rLeaves <- streamToHashSet_ $ getLeaves env Nothing Nothing

    let bounds = BranchBounds (HS.map LowerBound lLeaves) (HS.map UpperBound rLeaves)

    logg Debug "request remote branches limited by local leaves"

    -- We get remote headers in reverse order, so we have to buffer
    -- before we can start to insert in the local db. We could somewhat
    -- better by starting insertion as soon as we got a complete branch,
    -- but that's left as future optimization.
    getBranchHeaders env Nothing Nothing bounds
        & reverseStream
        & chunksOf 64 -- TODO: ideally, this would align with response pages
        & mapsM_ (insertStream ldb)

        -- FIXME avoid insertStream because of its problematic behavior on failure.
        -- FIXME add failure handling on a per block header basis

  where
    logg :: LogFunctionText
    logg = logFun

chainDbGenesisBlock :: BlockHeaderTreeDb db => db -> DbEntry db
chainDbGenesisBlock db = unsafePerformIO $ root db
{-# NOINLINE chainDbGenesisBlock #-}

chainDbChainwebVersion :: BlockHeaderTreeDb db => db -> ChainwebVersion
chainDbChainwebVersion = _blockChainwebVersion . chainDbGenesisBlock

chainDbChainId :: BlockHeaderTreeDb db => db -> ChainId
chainDbChainId = _blockChainId . chainDbGenesisBlock

chainClientEnv :: BlockHeaderTreeDb db => db -> ClientEnv -> ChainClientEnv
chainClientEnv db = ChainClientEnv
    (chainDbChainwebVersion db)
    (chainDbChainId db)

-- -------------------------------------------------------------------------- --
-- Sync Session

syncSession :: TreeDb db => (DbEntry db ~ BlockHeader) => db -> P2pSession
syncSession db logg env = do
    receiveBlockHeaders
    m <- maxHeader db
    race_
        (S.mapM_ send $ allEntries db (Just $ Exclusive $ key m))
        (forever $ receiveBlockHeaders >> threadDelay 5000000)
            -- FIXME make this configurable or dynamic

    -- this code must not be reached
    void $ logg @T.Text Error "unexpectedly exited sync session"
    return False
  where
    cenv = chainClientEnv db env

    send h = do
        putHeader cenv h
        logg Debug $ "put block header " <> showHash h

    receiveBlockHeaders = do
        logg @T.Text Info "start full sync"
        fullSync db logg cenv
        logg @T.Text Debug "finished full sync"

-- -------------------------------------------------------------------------- --
-- Utils

showHash :: Hashable a => a -> T.Text
showHash = T.pack . show . abs . hash
