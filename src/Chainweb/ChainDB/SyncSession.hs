{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Chainweb.ChainDB.SyncSession
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.ChainDB.SyncSession
( syncSession
) where

import Control.Lens ((&))
import Control.Monad
import Control.Monad.STM

import Data.Function
import Data.Functor.Of
import Data.Hashable
import Data.Maybe (fromJust)
import qualified Data.Text as T

import GHC.Generics

import Numeric.Natural

import Servant.Client

import Streaming
import qualified Streaming.Prelude as SP

import System.IO.Unsafe
import System.LogLevel

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.RestAPI.Client
import Chainweb.ChainId
import Chainweb.RestAPI.Utils
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Version

import Data.LogMessage

import P2P.Session

-- -------------------------------------------------------------------------- --
-- ChainDB Utils

data ChainClientEnv = ChainClientEnv
    { _envChainwebVersion :: !ChainwebVersion
    , _envChainId :: !ChainId
    , _envClientEnv :: !ClientEnv
    }
    deriving (Generic)

-- | TODO: add retry logic
--
runUnpaged :: ClientEnv -> (Maybe k -> ClientM (Page k a)) -> Stream (Of a) IO ()
runUnpaged env req = go Nothing
  where
    go k = lift (runClientThrowM (req k) env) >>= \page -> do
        SP.each (_pageItems page)
        when (_getEos $ _pageEos page) $ go $ _pageLastKey page

runClientThrowM :: ClientM a -> ClientEnv -> IO a
runClientThrowM req = fromEitherM <=< runClientM req

getHashes
    :: ChainClientEnv
    -> Maybe MinRank
    -> Maybe MaxRank
    -> Stream (Of (DbKey BlockHeaderDb)) IO ()
getHashes (ChainClientEnv v cid env) minr maxr = runUnpaged env $ \k ->
    hashesClient v cid Nothing k minr maxr

getBranches
    :: ChainClientEnv
    -> Maybe MinRank
    -> Maybe MaxRank
    -> Maybe (Bounds (DbKey BlockHeaderDb))
    -> Stream (Of (DbKey BlockHeaderDb)) IO ()
getBranches (ChainClientEnv v cid env) minr maxr range = runUnpaged env $ \k ->
    branchesClient v cid Nothing k minr maxr range

getHeaders
    :: ChainClientEnv
    -> Maybe MinRank
    -> Maybe MaxRank
    -> Stream (Of (DbEntry BlockHeaderDb)) IO ()
getHeaders (ChainClientEnv v cid env) minr maxr = runUnpaged env $ \k ->
    headersClient v cid Nothing k minr maxr

putHeader
    :: ChainClientEnv
    -> DbEntry BlockHeaderDb
    -> IO ()
putHeader (ChainClientEnv v cid env) = void . flip runClientThrowM env
    . headerPutClient v cid

-- -------------------------------------------------------------------------- --
-- Sync

-- TODO prune branches by rank. Ideally, the REST API would return
-- branches sorted in decending order by rank.
--
-- If we dont' get any know branch we have to fetch the first branch
-- limited by the genesis block. That's potentially expensive. In
-- this case we should traverse the first branch in chunks until
-- we find a known block header
--
fullSync :: BlockHeaderDb -> LogFunction -> ChainClientEnv -> IO ()
fullSync db logFun env = do pure ()  -- TODO restore

    -- logg Debug "request branches"
    -- sn <- snapshot db
    -- (unknownBranches :> maxKnownBranch) <- getBranches env Nothing Nothing
    --     & SP.map (\k -> maybe (Right k) Left $ lookupEntry k sn)
    --     & SP.partitionEithers
    --     & SP.fold_ (maxBy (compare `on` rank)) (chainDbGenesisEntry db) id
    --     & SP.toList

    -- logg Debug $ "got " <> sshow (length unknownBranches) <> " unknown branches"
    -- logg Debug $ "maximum known branch is " <> sshow maxKnownBranch

    -- we break streaming here and buffer all branches, because we want
    -- to continue with the maximum known branch. This can safe a lot of
    -- data in the requests below.

    -- logg Debug "request headers"
    -- unknownBranches
    --     & SP.each
    --         -- note that the the order of branches is reversed
    --     & SP.foldM_ fetchHeaders
    --         (return . uncheckedEntry $ maxKnownBranch)
    --         (void . return)

    -- TODO count received headers

  -- where
  --   fetchHeaders :: DbEntry BlockHeaderDb -> DbKey BlockHeaderDb -> IO (DbEntry BlockHeaderDb)
  --   fetchHeaders curMax b = getHeaders env Nothing Nothing (Just (key curMax, b))
  --       & SP.copy
  --       & SP.foldM
  --           (\sn -> lift . flip insert sn)
  --           (lift $ snapshot db)
  --           (lift . syncSnapshot)
  --       & SP.fold_ (maxBy (compare `on` rank)) curMax id

  --   logg :: LogFunctionText
  --   logg = logFun

chainDbGenesisBlock :: BlockHeaderDb -> DbEntry BlockHeaderDb
chainDbGenesisBlock db = fromJust . unsafePerformIO $ do
    SP.head_ $ entries db Nothing Nothing Nothing Nothing

chainDbChainwebVersion :: BlockHeaderDb -> ChainwebVersion
chainDbChainwebVersion = _blockChainwebVersion . chainDbGenesisBlock

chainDbChainId :: BlockHeaderDb -> ChainId
chainDbChainId = _blockChainId . chainDbGenesisBlock

chainClientEnv :: BlockHeaderDb -> ClientEnv -> ChainClientEnv
chainClientEnv db = ChainClientEnv
    (chainDbChainwebVersion db)
    (chainDbChainId db)

-- -------------------------------------------------------------------------- --
-- Sync Session

syncSession :: BlockHeaderDb -> P2pSession
syncSession db logg env = pure True
  -- TODO restore
  -- where
  --   cenv = chainClientEnv db env

  --   go = do
  --       hashes <- updates db
  --       atomically $ drainUpdates hashes
  --       receiveBlockHeaders
  --       atomically $ drainUpdates hashes
  --       sendAllBlockHeaders hashes

  --   sendAllBlockHeaders hashes = forever $ do
  --       s <- atomically $ updatesNext hashes
  --       dbs <- snapshot db
  --       e <- getEntryIO s dbs
  --       putHeader cenv (uncheckedEntry e)
  --       logg Debug $ "put block header " <> showHash s

  --   receiveBlockHeaders = do
  --       fullSync db logg cenv
  --       logg @T.Text Debug "finished full sync"

-- drainUpdates :: Updates -> STM ()
-- drainUpdates u = go
--   where
--     go = updatesNext u *> go <|> return ()

-- -------------------------------------------------------------------------- --
-- Utils

showHash :: Hashable a => a -> T.Text
showHash = T.pack . show . abs . hash
