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
import Chainweb.CutDB (CutDb, blockDiffStream, cutDbPayloadDb)
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
    :: BlockHeaderDb
    -> Maybe Limit
    -> Maybe (NextItem (DbKey BlockHeaderDb))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> Handler (Page (NextItem (DbKey BlockHeaderDb)) (DbKey BlockHeaderDb))
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
    :: CanReadablePayloadCas tbl
    => BlockHeaderDb
    -> PayloadDb tbl
    -> Maybe Limit
    -> Maybe (NextItem (DbKey BlockHeaderDb))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> Bool
    -> BranchBounds BlockHeaderDb
    -> Handler (Page (NextItem (DbKey BlockHeaderDb)) BlockHeaderResult)
branchHeadersHandler db pdb limit next minr maxr includePayload bounds = do
    nextChecked <- traverse (traverse $ checkKey db) next
    checkedBounds <- checkBounds db bounds
    liftIO
        $ branchEntries db nextChecked (succ <$> effectiveLimit) minr maxr
            (_branchBoundsLower checkedBounds)
            (_branchBoundsUpper checkedBounds)
        $ finiteStreamToPage (key.resultHeader) effectiveLimit . void . SP.mapM (fetchPayload pdb includePayload)
  where
    effectiveLimit = min defaultEntryLimit <$> (limit <|> Just defaultEntryLimit)

fetchPayload :: CanReadablePayloadCas tbl => PayloadDb tbl -> Bool -> BlockHeader -> IO BlockHeaderResult
fetchPayload pdb True bh = do
    Just payload <- lookupPayloadWithHeight pdb (_blockHeight bh) (_blockPayloadHash bh)
    let pd = payloadWithOutputsToPayloadData payload
    return (BlockHeaderResult bh (Just pd))
fetchPayload _ False bh =
    return (BlockHeaderResult bh Nothing)

-- | Every `TreeDb` key within a given range.
--
-- Cf. "Chainweb.BlockHeaderDB.RestAPI" for more details
--
hashesHandler
    :: BlockHeaderDb
    -> Maybe Limit
    -> Maybe (NextItem (DbKey BlockHeaderDb))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> Handler (Page (NextItem (DbKey BlockHeaderDb)) (DbKey BlockHeaderDb))
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
    :: CanReadablePayloadCas tbl
    => BlockHeaderDb
    -> PayloadDb tbl
    -> Maybe Limit
    -> Maybe (NextItem (DbKey BlockHeaderDb))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> Bool
    -> Handler (Page (NextItem (DbKey BlockHeaderDb)) BlockHeaderResult)
headersHandler db pdb limit next minr maxr includePayload = do
    nextChecked <- traverse (traverse $ checkKey db) next
    liftIO
        $ entries db nextChecked (succ <$> effectiveLimit) minr maxr
        $ finitePrefixOfInfiniteStreamToPage (key.resultHeader) effectiveLimit . void . SP.mapM (fetchPayload pdb includePayload)
  where
    effectiveLimit = min defaultEntryLimit <$> (limit <|> Just defaultEntryLimit)

-- | Query a single 'BlockHeader' by its 'BlockHash'
--
-- Cf. "Chainweb.BlockHeaderDB.RestAPI" for more details
--
headerHandler
    :: CanReadablePayloadCas tbl
    => BlockHeaderDb
    -> PayloadDb tbl
    -> Bool
    -> DbKey BlockHeaderDb
    -> Handler BlockHeaderResult
headerHandler db pdb includePayload k = liftIO (lookup db k) >>= \case
    Nothing -> throwError $ err404Msg $ object
        [ "reason" .= ("key not found" :: String)
        , "key" .= k
        ]
    Just e ->
        liftIO $ fetchPayload pdb includePayload e

-- -------------------------------------------------------------------------- --
-- BlockHeaderDB API Server

blockHeaderDbServer :: CanReadablePayloadCas tbl => BlockHeaderDb_ v c -> PayloadDb tbl -> Server (BlockHeaderDbApi v c)
blockHeaderDbServer (BlockHeaderDb_ db) pdb
    = hashesHandler db
    :<|> headersHandler db pdb
    :<|> headerHandler db pdb
    :<|> branchHashesHandler db
    :<|> branchHeadersHandler db pdb

-- -------------------------------------------------------------------------- --
-- Application for a single BlockHeaderDB

blockHeaderDbApp
    :: forall v c tbl
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => CanReadablePayloadCas tbl
    => BlockHeaderDb_ v c
    -> PayloadDb tbl
    -> Application
blockHeaderDbApp bhdb pdb = serve (Proxy @(BlockHeaderDbApi v c)) (blockHeaderDbServer bhdb pdb)

blockHeaderDbApiLayout
    :: forall v c
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => BlockHeaderDb_ v c
    -> IO ()
blockHeaderDbApiLayout _ = T.putStrLn $ layout (Proxy @(BlockHeaderDbApi v c))

-- -------------------------------------------------------------------------- --
-- Multichain Server

someBlockHeaderDbServer :: CanReadablePayloadCas tbl => SomeBlockHeaderDb -> PayloadDb tbl -> SomeServer
someBlockHeaderDbServer (SomeBlockHeaderDb (db :: BlockHeaderDb_ v c)) pdb
    = SomeServer (Proxy @(BlockHeaderDbApi v c)) (blockHeaderDbServer db pdb)

someBlockHeaderDbServers :: CanReadablePayloadCas tbl => ChainwebVersion -> [(ChainId, BlockHeaderDb, PayloadDb tbl)] -> SomeServer
someBlockHeaderDbServers v = mconcat
    . fmap (\(cid,bhdb,pdb) -> someBlockHeaderDbServer (someBlockHeaderDbVal v cid bhdb) pdb)

-- -------------------------------------------------------------------------- --
-- BlockHeader Event Stream

someHeaderStreamServer :: CanReadablePayloadCas tbl => ChainwebVersion -> CutDb tbl -> SomeServer
someHeaderStreamServer (FromSingChainwebVersion (SChainwebVersion :: Sing v)) cdb =
    SomeServer (Proxy @(HeaderStreamApi v)) $ headerStreamServer cdb

headerStreamServer
    :: forall tbl (v :: ChainwebVersionT)
    .  CanReadablePayloadCas tbl
    => CutDb tbl
    -> Server (HeaderStreamApi v)
headerStreamServer cdb = headerStreamHandler cdb

headerStreamHandler :: forall tbl. CanReadablePayloadCas tbl => CutDb tbl -> Tagged Handler Application
headerStreamHandler db = Tagged $ \req resp -> do
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
        Just x <- lookupPayloadWithHeight cas (_blockHeight bh) $ _blockPayloadHash bh
        pure $ HeaderUpdate
            { _huHeader =  ObjectEncoded bh
            , _huTxCount = length $ _payloadWithOutputsTransactions x
            , _huPowHash = decodeUtf8 . B16.encode . BS.reverse . fromShort . powHashBytes $ _blockPow bh
            , _huTarget = showTargetHex $ _blockTarget bh }

    f :: HeaderUpdate -> ServerEvent
    f hu = ServerEvent (Just $ fromByteString "BlockHeader") Nothing
        [ fromLazyByteString . encode $ toJSON hu ]
