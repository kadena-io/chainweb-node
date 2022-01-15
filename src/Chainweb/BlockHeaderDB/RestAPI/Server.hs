{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

-- * Single Chain Server
, blockHeaderDbApp
, blockHeaderDbApiLayout

-- * Header Stream Server
, someHeaderStreamServer
) where

import Control.Applicative
import Control.Lens hiding (children, (.=))
import Control.Monad
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class

import Data.Aeson
import Data.Binary.Builder (fromByteString, fromLazyByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Short (fromShort)
import Data.CAS (casLookupM)
import Data.Foldable
import Data.Functor.Of
import Data.IORef
import Data.Proxy
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T

import Network.Wai.EventSource (ServerEvent(..), eventSourceAppIO)

import Prelude hiding (lookup)

import Servant.API
import Servant.Server

import qualified Streaming.Prelude as SP

-- internal modules

import Chainweb.BlockHeader (BlockHeader(..), ObjectEncoded(..), _blockPow)
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.RestAPI
import Chainweb.ChainId
import Chainweb.CutDB (CutDb, blockDiffStream, cutDbPayloadCas)
import Chainweb.Difficulty (showTargetHex)
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.PowHash (powHashBytes)
import Chainweb.RestAPI.Orphans ()
import Chainweb.RestAPI.Utils
import Chainweb.TreeDB
import Chainweb.Utils.Paging
import Chainweb.Version

-- -------------------------------------------------------------------------- --
-- Handler Tools

checkKey
    :: MonadError ServerError m
    => MonadIO m
    => TreeDb db
    => ToJSON (DbKey db)
    => db
    -> DbKey db
    -> m (DbKey db)
checkKey !db !k = liftIO (lookup db k) >>= \case
    Nothing -> throwError $ err404Msg $ object
        [ "reason" .= ("key not found" :: String)
        , "key" .= k
        ]
    Just _ -> pure k

err404Msg :: ToJSON msg  => msg -> ServerError
err404Msg msg = ServerError
    { errHTTPCode = 404
    , errReasonPhrase = "Not Found"
    , errBody = encode msg
    , errHeaders = []
    }

-- | Confirm if keys comprising the given bounds exist within a `TreeDb`.
--
checkBounds
    :: MonadError ServerError m
    => MonadIO m
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

-- | Query Branch Hashes of the database.
--
-- Cf. "Chainweb.BlockHeaderDB.RestAPI" for more details
--
branchHashesHandler
    :: TreeDb db
    => ToJSON (DbKey db)
    => db
    -> Maybe Limit
    -> Maybe (NextItem (DbKey db))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds db
    -> Handler (Page (NextItem (DbKey db)) (DbKey db))
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
    => ToJSON (DbKey db)
    => db
    -> Maybe Limit
    -> Maybe (NextItem (DbKey db))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds db
    -> Handler (Page (NextItem (DbKey db)) (DbEntry db))
branchHeadersHandler db limit next minr maxr bounds = do
    nextChecked <- traverse (traverse $ checkKey db) next
    checkedBounds <- checkBounds db bounds
    liftIO
        $ branchEntries db nextChecked (succ <$> effectiveLimit) minr maxr
            (_branchBoundsLower checkedBounds)
            (_branchBoundsUpper checkedBounds)
        $ finiteStreamToPage key effectiveLimit . void
  where
    effectiveLimit = min defaultEntryLimit <$> (limit <|> Just defaultEntryLimit)

-- | Every `TreeDb` key within a given range.
--
-- Cf. "Chainweb.BlockHeaderDB.RestAPI" for more details
--
hashesHandler
    :: TreeDb db
    => ToJSON (DbKey db)
    => db
    -> Maybe Limit
    -> Maybe (NextItem (DbKey db))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> Handler (Page (NextItem (DbKey db)) (DbKey db))
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
    => ToJSON (DbKey db)
    => db
    -> Maybe Limit
    -> Maybe (NextItem (DbKey db))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> Handler (Page (NextItem (DbKey db)) (DbEntry db))
headersHandler db limit next minr maxr = do
    nextChecked <- traverse (traverse $ checkKey db) next
    liftIO
        $ entries db nextChecked (succ <$> effectiveLimit) minr maxr
        $ finitePrefixOfInfiniteStreamToPage key effectiveLimit . void
  where
    effectiveLimit = min defaultEntryLimit <$> (limit <|> Just defaultEntryLimit)

-- | Query a single 'BlockHeader' by its 'BlockHash'
--
-- Cf. "Chainweb.BlockHeaderDB.RestAPI" for more details
--
headerHandler
    :: ToJSON (DbKey db)
    => TreeDb db
    => db
    -> DbKey db
    -> Handler (DbEntry db)
headerHandler db k = liftIO (lookup db k) >>= \case
    Nothing -> throwError $ err404Msg $ object
        [ "reason" .= ("key not found" :: String)
        , "key" .= k
        ]
    Just e -> pure e

-- -------------------------------------------------------------------------- --
-- BlockHeaderDB API Server

blockHeaderDbServer :: BlockHeaderDb_ v c -> Server (BlockHeaderDbApi v c)
blockHeaderDbServer (BlockHeaderDb_ db)
    = hashesHandler db
    :<|> headersHandler db
    :<|> headerHandler db
    :<|> branchHashesHandler db
    :<|> branchHeadersHandler db

-- -------------------------------------------------------------------------- --
-- Application for a single BlockHeaderDB

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

-- -------------------------------------------------------------------------- --
-- BlockHeader Event Stream

someHeaderStreamServer :: PayloadCasLookup cas => ChainwebVersion -> CutDb cas -> SomeServer
someHeaderStreamServer (FromSingChainwebVersion (SChainwebVersion :: Sing v)) cdb =
    SomeServer (Proxy @(HeaderStreamApi v)) $ headerStreamServer cdb

headerStreamServer
    :: forall cas (v :: ChainwebVersionT)
    .  PayloadCasLookup cas
    => CutDb cas
    -> Server (HeaderStreamApi v)
headerStreamServer cdb = headerStreamHandler cdb

headerStreamHandler :: forall cas. PayloadCasLookup cas => CutDb cas -> Tagged Handler Application
headerStreamHandler db = Tagged $ \req resp -> do
    streamRef <- newIORef $ SP.map f $ SP.mapM g $ SP.concat $ blockDiffStream db
    eventSourceAppIO (run streamRef) req resp
  where
    run :: IORef (SP.Stream (Of ServerEvent) IO ()) -> IO ServerEvent
    run var = readIORef var >>= SP.uncons >>= \case
        Nothing -> return CloseEvent
        Just (cur, !s') -> cur <$ writeIORef var s'

    cas :: PayloadDb cas
    cas = view cutDbPayloadCas db

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
