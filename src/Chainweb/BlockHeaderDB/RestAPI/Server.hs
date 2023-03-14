{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Chainweb.BlockHeaderDB.RestAPI.Server
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Server implementation of the BlockHeader database REST API
--
module Chainweb.BlockHeaderDB.RestAPI.Server
(
  someBlockHeaderDbServer
, someBlockHeaderDbServers
, BlockHeaderDbServerOptions(..)
, newBlockHeaderDbServer

-- * Single Chain Server
, blockHeaderDbApp
, blockHeaderDbApiLayout

-- * Header Stream Server
, someHeaderStreamServer
, headerStreamServer
) where

import Control.Applicative
import Control.Lens hiding (children, (.=))
import Control.Monad
import Control.Monad.IO.Class

import Data.Aeson
import Data.Binary.Builder (fromByteString, fromLazyByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Short (fromShort)
import Data.Foldable
import Data.Functor.Of
import Data.IORef
import Data.Maybe
import Data.Proxy
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T

import Network.HTTP.Types
import qualified Network.Wai as Wai
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppIO)

import Prelude hiding (lookup)

import Servant.API hiding (QueryParam)
import Servant.Server

import qualified Streaming.Prelude as SP

import Web.DeepRoute
import Web.DeepRoute.Wai

-- internal modules

import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.RestAPI
import Chainweb.ChainId
import Chainweb.CutDB (CutDb, blockDiffStream, cutDbPayloadDb)
import Chainweb.Difficulty (showTargetHex)
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.PowHash (powHashBytes)
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.TreeDB
import Chainweb.Utils.Paging
import Chainweb.Utils.Serialization
import Chainweb.Version

import Chainweb.Storage.Table

-- -------------------------------------------------------------------------- --
-- Handler Tools

checkKey
    :: MonadIO m
    => TreeDb db
    => ToJSON (DbKey db)
    => db
    -> DbKey db
    -> m (DbKey db)
checkKey !db !k = liftIO $ lookup db k >>= \case
    Nothing -> jsonErrorWithStatus notFound404 $ object
        [ "reason" .= ("key not found" :: String)
        , "key" .= k
        ]
    Just _ -> pure k

-- | Confirm if keys comprising the given bounds exist within a `TreeDb`.
--
checkBounds
    :: MonadIO m
    => ToJSON (DbKey db)
    => TreeDb db
    => db
    -> BranchBounds db
    -> m (BranchBounds db)
checkBounds db b = b
    <$ traverse_ (checkKey db . _getUpperBound) (_branchBoundsUpper b)

-- -------------------------------------------------------------------------- --
-- Handlers

defaultKeyLimit :: Num a => a
defaultKeyLimit = 4096

defaultEntryLimit :: Num a => a
defaultEntryLimit = 360

p2pEntryLimit :: Num a => a
p2pEntryLimit = 20

-- | Query Branch Hashes of the database.
--
-- Cf. "Chainweb.BlockHeaderDB.RestAPI" for more details
--
branchHashesHandler
    :: (TreeDb db, MonadIO m)
    => ToJSON (DbKey db)
    => db
    -> Maybe Limit
    -> Maybe (NextItem (DbKey db))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds db
    -> m (Page (NextItem (DbKey db)) (DbKey db))
branchHashesHandler db limit next minr maxr bounds = do
    nextChecked <- traverse (traverse $ checkKey db) next
    checkedBounds <- checkBounds db bounds
    liftIO
        $ branchKeys db nextChecked (succ <$> effectiveLimit) minr maxr
            (_branchBoundsLower checkedBounds)
            (_branchBoundsUpper checkedBounds)
        $ finiteStreamToPage id effectiveLimit . void
  where
    effectiveLimit = min defaultKeyLimit <$> (limit <|> Just defaultKeyLimit)

-- | Query Branch Headers of the database.
--
-- Cf. "Chainweb.BlockHeaderDB.RestAPI" for more details
--
branchHeadersHandler
    :: TreeDb db
    => MonadIO m
    => ToJSON (DbKey db)
    => db
    -> Limit
        -- ^ max limit
    -> Maybe Limit
    -> Maybe (NextItem (DbKey db))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds db
    -> m (Page (NextItem (DbKey db)) (DbEntry db))
branchHeadersHandler db maxLimit limit next minr maxr bounds = do
    nextChecked <- traverse (traverse $ checkKey db) next
    checkedBounds <- checkBounds db bounds
    liftIO
        $ branchEntries db nextChecked (succ <$> effectiveLimit) minr maxr
            (_branchBoundsLower checkedBounds)
            (_branchBoundsUpper checkedBounds)
        $ finiteStreamToPage key effectiveLimit . void
  where
    effectiveLimit = min maxLimit <$> (limit <|> Just maxLimit)

-- | Every `TreeDb` key within a given range.
--
-- Cf. "Chainweb.BlockHeaderDB.RestAPI" for more details
--
hashesHandler
    :: TreeDb db
    => MonadIO m
    => ToJSON (DbKey db)
    => db
    -> Maybe Limit
    -> Maybe (NextItem (DbKey db))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> m (Page (NextItem (DbKey db)) (DbKey db))
hashesHandler db limit next minr maxr = do
    nextChecked <- traverse (traverse $ checkKey db) next
    liftIO
        $ keys db nextChecked (succ <$> effectiveLimit) minr maxr
        $ finitePrefixOfInfiniteStreamToPage id effectiveLimit
        . void
  where
    effectiveLimit = min defaultKeyLimit <$> (limit <|> Just defaultKeyLimit)

-- | Every `TreeDb` entry within a given range.
--
-- Cf. "Chainweb.BlockHeaderDB.RestAPI" for more details
--
headersHandler
    :: TreeDb db
    => MonadIO m
    => ToJSON (DbKey db)
    => db
    -> Limit
        -- ^ max limit
    -> Maybe Limit
    -> Maybe (NextItem (DbKey db))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> m (Page (NextItem (DbKey db)) (DbEntry db))
headersHandler db maxLimit limit next minr maxr = do
    nextChecked <- traverse (traverse $ checkKey db) next
    liftIO
        $ entries db nextChecked (succ <$> effectiveLimit) minr maxr
        $ finitePrefixOfInfiniteStreamToPage key effectiveLimit . void
  where
    effectiveLimit = min maxLimit <$> (limit <|> Just maxLimit)

-- | Query a single 'BlockHeader' by its 'BlockHash'
--
-- Cf. "Chainweb.BlockHeaderDB.RestAPI" for more details
--
headerHandler
    :: MonadIO m
    => ToJSON (DbKey db)
    => TreeDb db
    => db
    -> DbKey db
    -> m (DbEntry db)
headerHandler db k = liftIO $ lookup db k >>= \case
    Nothing -> jsonErrorWithStatus notFound404 $ object
        [ "reason" .= ("key not found" :: String)
        , "key" .= k
        ]
    Just e -> pure e

-- -------------------------------------------------------------------------- --
-- BlockHeaderDB API Server

-- Full BlockHeader DB API (used for Service API)
--
blockHeaderDbServer :: BlockHeaderDb_ v c -> Server (BlockHeaderDbApi v c)
blockHeaderDbServer (BlockHeaderDb_ db)
    = hashesHandler db
    :<|> headersHandler db defaultEntryLimit
    :<|> headerHandler db
    :<|> branchHashesHandler db
    :<|> branchHeadersHandler db defaultEntryLimit

-- -------------------------------------------------------------------------- --
-- Application for a single BlockHeaderDB

data BlockHeaderDbServerOptions = BlockHeaderDbServerOptions
    { enableHashesEndpoints :: !Bool
    , entryLimit :: !Limit
    }

newBlockHeaderDbServer
    :: BlockHeaderDbServerOptions -> Route (BlockHeaderDb -> Application)
newBlockHeaderDbServer opts = fold
    [ fold
        [ choice "hash" $ fold
            [ choice "branch" $ terminus methodPost "application/json" $ \db req resp -> do
                ((limit, next), (minheight, maxheight)) <-
                    getParams req $ (,) <$> pageParams <*> filterParams
                resp . responseJSON ok200 [] =<<
                    branchHashesHandler db limit next minheight maxheight =<< requestFromJSON req
            , terminus methodGet "application/json" $ \db req resp -> do
                ((limit, next), (minheight, maxheight)) <-
                    getParams req $ (,) <$> pageParams <*> filterParams
                resp . responseJSON ok200 [] =<<
                    hashesHandler db limit next minheight maxheight
            ]
        | enableHashesEndpoints opts
        ]
    , choice "header" $ fold
        [ choice "branch" $ terminus'
            [ (methodPost, "application/json",) $ \db req resp -> do
                ((limit, next), (minheight, maxheight)) <-
                    getParams req $ (,) <$> pageParams <*> filterParams
                resp . responseJSON ok200 [] =<<
                    branchHeadersHandler db (entryLimit opts) limit next minheight maxheight =<< requestFromJSON req
            , (methodPost, "application/json;blockheader-encoding=object",) $ \db req resp -> do
                ((limit, next), (minheight, maxheight)) <-
                    getParams req $ (,) <$> pageParams <*> filterParams
                resp . responseJSON ok200 [] . fmap ObjectEncoded =<<
                    branchHeadersHandler db (entryLimit opts) limit next minheight maxheight =<< requestFromJSON req
            ]
        , terminus'
            [ (methodGet, "application/json",) $ \db req resp -> do
                ((limit, next), (minheight, maxheight)) <-
                    getParams req $ (,) <$> pageParams <*> filterParams
                resp . responseJSON ok200 [] =<<
                    headersHandler db (entryLimit opts) limit next minheight maxheight
            , (methodGet, "application/json;blockheader-encoding=object",) $ \db req resp -> do
                ((limit, next), (minheight, maxheight)) <-
                    getParams req $ (,) <$> pageParams <*> filterParams
                resp . responseJSON ok200 [] . fmap ObjectEncoded =<<
                    headersHandler db (entryLimit opts) limit next minheight maxheight
            ]
        , capture $ terminus'
            [ (methodGet, "application/json",) $ \bh db _ resp -> do
                resp . responseJSON ok200 [] =<<
                    headerHandler db bh
            , (methodGet, "application/json;blockheader-encoding=object",) $ \bh db _ resp ->
                resp . responseJSON ok200 [] . ObjectEncoded =<<
                    headerHandler db bh
            , (methodGet, "application/octet-stream",) $ \bh db _ resp ->
                resp . Wai.responseLBS ok200 [] . runPutL . encodeBlockHeader =<<
                    headerHandler db bh
            ]
        ]
    ]
    where
    pageParams = (,) <$> limitParam <*> nextParam
    limitParam = queryParamMaybe "limit"
    nextParam = queryParamMaybe "next"
    minHeightParam = queryParamMaybe "minheight"
    maxHeightParam = queryParamMaybe "maxheight"
    filterParams = (,) <$> minHeightParam <*> maxHeightParam

blockHeaderDbApp
    :: forall v c
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => BlockHeaderDb_ v c
    -> Application
blockHeaderDbApp db = serve (Proxy @(BlockHeaderDbApi v c)) (blockHeaderDbServer db)

blockHeaderDbApiLayout
    :: forall v c
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => BlockHeaderDb_ v c
    -> IO ()
blockHeaderDbApiLayout _ = T.putStrLn $ layout (Proxy @(BlockHeaderDbApi v c))

-- -------------------------------------------------------------------------- --
-- Multichain Server

someBlockHeaderDbServer :: SomeBlockHeaderDb -> SomeServer
someBlockHeaderDbServer (SomeBlockHeaderDb (db :: BlockHeaderDb_ v c))
    = SomeServer (Proxy @(BlockHeaderDbApi v c)) (blockHeaderDbServer db)

someBlockHeaderDbServers :: ChainwebVersion -> [(ChainId, BlockHeaderDb)] -> SomeServer
someBlockHeaderDbServers v = mconcat
    . fmap (someBlockHeaderDbServer . uncurry (someBlockHeaderDbVal v))

-- someP2pBlockHeaderDbServer :: SomeBlockHeaderDb -> SomeServer
-- someP2pBlockHeaderDbServer (SomeBlockHeaderDb (db :: BlockHeaderDb_ v c))
--     = SomeServer (Proxy @(P2pBlockHeaderDbApi v c)) (p2pBlockHeaderDbServer db)

-- someP2pBlockHeaderDbServers :: ChainwebVersion -> [(ChainId, BlockHeaderDb)] -> SomeServer
-- someP2pBlockHeaderDbServers v = mconcat
--     . fmap (someP2pBlockHeaderDbServer . uncurry (someBlockHeaderDbVal v))

-- -------------------------------------------------------------------------- --
-- BlockHeader Event Stream

headerStreamServer :: CanReadablePayloadCas tbl => CutDb tbl -> Route Wai.Application
headerStreamServer db =
    choice "header" $
        choice "updates" $
            terminus methodGet "text/event-stream" $ headerStreamHandler db

someHeaderStreamServer :: CanReadablePayloadCas tbl => ChainwebVersion -> CutDb tbl -> SomeServer
someHeaderStreamServer (FromSingChainwebVersion (SChainwebVersion :: Sing v)) cdb =
    SomeServer (Proxy @(HeaderStreamApi v)) $ Tagged $ headerStreamHandler cdb

headerStreamHandler :: forall tbl. CanReadablePayloadCas tbl => CutDb tbl -> Wai.Application
headerStreamHandler db = \req resp -> do
    streamRef <- newIORef $ SP.map f $ SP.mapM g $ SP.concat $ blockDiffStream db
    eventSourceAppIO (run streamRef) req resp
  where
    run :: IORef (SP.Stream (Of ServerEvent) IO ()) -> IO ServerEvent
    run var = readIORef var >>= SP.uncons >>= \case
        Nothing -> return CloseEvent
        Just (cur, !s') -> cur <$ writeIORef var s'

    cas :: PayloadDb tbl
    cas = view cutDbPayloadDb db

    g :: BlockHeader -> IO HeaderUpdate
    g bh = do
        x <- casLookupM cas $ _blockPayloadHash bh
        pure $ HeaderUpdate
            { _huHeader =  ObjectEncoded bh
            , _huTxCount = length $ _payloadWithOutputsTransactions x
            , _huPowHash = decodeUtf8 . B16.encode . BS.reverse . fromShort . powHashBytes $ _blockPow bh
            , _huTarget = showTargetHex $ _blockTarget bh }

    f :: HeaderUpdate -> ServerEvent
    f hu = ServerEvent (Just $ fromByteString "BlockHeader") Nothing
        [ fromLazyByteString . encode $ toJSON hu ]
