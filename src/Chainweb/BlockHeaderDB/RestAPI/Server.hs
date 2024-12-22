{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
, someP2pBlockHeaderDbServer
, someP2pBlockHeaderDbServers

-- * Header Stream Server
, someBlockStreamServer
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
import Data.Function
import Data.Functor.Of
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Text.Encoding (decodeUtf8)
import Numeric.Natural(Natural)

import Network.Wai.EventSource (ServerEvent(..), eventSourceAppIO)

import Prelude hiding (lookup)

import Servant.API
import Servant.Server

import qualified Streaming.Prelude as SP

-- internal modules

import Chainweb.BlockHash
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
import Chainweb.Version
import Chainweb.Block

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

err404Msg :: ToJSON msg => msg -> ServerError
err404Msg msg = setErrJSON msg err404

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

err400Msg :: ToJSON msg  => msg -> ServerError
err400Msg msg = ServerError
    { errHTTPCode = 400
    , errReasonPhrase = "Bad request"
    , errBody = encode msg
    , errHeaders = []
    }

-- -------------------------------------------------------------------------- --
-- Handlers

defaultKeyLimit :: Num a => a
defaultKeyLimit = 4096

defaultEntryLimit :: Num a => a
defaultEntryLimit = 360

p2pEntryLimit :: Num a => a
p2pEntryLimit = 20

newtype BranchBoundsLimit
    = BranchBoundsLimit { getBranchBoundsLimit :: Natural }
    deriving newtype (Show, Eq, Ord)

-- | Default limit for the number of bounds in the request of a branch query
--
defaultBoundsLimit :: BranchBoundsLimit
defaultBoundsLimit = BranchBoundsLimit 32

-- | Limit for the number of bounds in the request of a branch query on the P2P
-- API.
--
p2pBoundsLimit :: BranchBoundsLimit
p2pBoundsLimit = BranchBoundsLimit 4

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
branchHashesHandler db limit next minr maxr bounds
    | fromIntegral (length (_branchBoundsUpper bounds)) > getBranchBoundsLimit defaultBoundsLimit = throwError $ err400Msg $
        "upper branch bound limit exceeded. Only " <> show defaultBoundsLimit <> " values are supported."
    | fromIntegral (length (_branchBoundsLower bounds)) > getBranchBoundsLimit defaultBoundsLimit = throwError $ err400Msg $
        "lower branch bound limit exceeded. Only " <> show defaultBoundsLimit <> " values are supported."
    | otherwise = do
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
    -> BranchBoundsLimit
    -> Limit
        -- ^ max limit
    -> Maybe Limit
    -> Maybe (NextItem (DbKey db))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds db
    -> Handler (Page (NextItem (DbKey db)) (DbEntry db))
branchHeadersHandler db (BranchBoundsLimit boundsLimit) maxLimit limit next minr maxr bounds
    | fromIntegral (length (_branchBoundsUpper bounds)) > boundsLimit = throwError $ err400Msg $
        "upper branch bound limit exceeded. Only " <> show boundsLimit <> " values are supported."
    | fromIntegral (length (_branchBoundsLower bounds)) > boundsLimit = throwError $ err400Msg $
        "lower branch bound limit exceeded. Only " <> show boundsLimit <> " values are supported."
    | otherwise = do
        nextChecked <- traverse (traverse $ checkKey db) next
        checkedBounds <- checkBounds db bounds
        liftIO
            $ branchEntries db nextChecked (succ <$> effectiveLimit) minr maxr
                (_branchBoundsLower checkedBounds)
                (_branchBoundsUpper checkedBounds)
            $ finiteStreamToPage key effectiveLimit . void
  where
    effectiveLimit = min maxLimit <$> (limit <|> Just maxLimit)

-- | Query Branch Blocks of the database.
--
-- Cf. "Chainweb.BlockHeaderDB.RestAPI" for more details
--
branchBlocksHandler
    :: CanReadablePayloadCas tbl
    => BlockHeaderDb
    -> PayloadDb tbl
    -> BranchBoundsLimit
    -> Limit
        -- ^ max limit
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> BranchBounds BlockHeaderDb
    -> Handler (Page (NextItem BlockHash) Block)
branchBlocksHandler bhdb pdb (BranchBoundsLimit boundsLimit) maxLimit limit next minr maxr bounds
    | fromIntegral (length (_branchBoundsUpper bounds)) > boundsLimit = throwError $ err400Msg $
        "upper branch bound limit exceeded. Only " <> show boundsLimit <> " values are supported."
    | fromIntegral (length (_branchBoundsLower bounds)) > boundsLimit = throwError $ err400Msg $
        "lower branch bound limit exceeded. Only " <> show boundsLimit <> " values are supported."
    | otherwise = do
        nextChecked <- traverse (traverse $ checkKey bhdb) next
        checkedBounds <- checkBounds bhdb bounds
        liftIO
            $ branchEntries bhdb nextChecked (succ <$> effectiveLimit) minr maxr
                (_branchBoundsLower checkedBounds)
                (_branchBoundsUpper checkedBounds)
            $ finiteStreamToPage (key . _blockHeader) effectiveLimit . void . SP.mapM grabPayload
  where
    effectiveLimit = min maxLimit <$> (limit <|> Just maxLimit)
    grabPayload :: BlockHeader -> IO Block
    grabPayload h = do
        Just x <- lookupPayloadWithHeight pdb (Just $ view blockHeight h) (view blockPayloadHash h)
        pure (Block h x)

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
    -> Limit
        -- ^ max limit
    -> Maybe Limit
    -> Maybe (NextItem (DbKey db))
    -> Maybe MinRank
    -> Maybe MaxRank
    -> Handler (Page (NextItem (DbKey db)) (DbEntry db))
headersHandler db maxLimit limit next minr maxr = do
    nextChecked <- traverse (traverse $ checkKey db) next
    liftIO
        $ entries db nextChecked (succ <$> effectiveLimit) minr maxr
        $ finitePrefixOfInfiniteStreamToPage key effectiveLimit . void
  where
    effectiveLimit = min maxLimit <$> (limit <|> Just maxLimit)

-- | Every block within a given range.
--
-- Cf. "Chainweb.BlockHeaderDB.RestAPI" for more details
--
blocksHandler
    :: CanReadablePayloadCas tbl
    => BlockHeaderDb
    -> PayloadDb tbl
    -> Limit
        -- ^ max limit
    -> Maybe Limit
    -> Maybe (NextItem BlockHash)
    -> Maybe MinRank
    -> Maybe MaxRank
    -> Handler BlockPage
blocksHandler bhdb pdb maxLimit limit next minr maxr = do
    nextChecked <- traverse (traverse $ checkKey bhdb) next
    liftIO
        $ entries bhdb nextChecked (succ <$> effectiveLimit) minr maxr
        $ finitePrefixOfInfiniteStreamToPage (key . _blockHeader) effectiveLimit . void . SP.mapM grabPayload
  where
    effectiveLimit = min maxLimit <$> (limit <|> Just maxLimit)
    grabPayload :: BlockHeader -> IO Block
    grabPayload h = do
        Just x <- lookupPayloadWithHeight pdb (Just $ view blockHeight h) (view blockPayloadHash h)
        pure (Block h x)

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

-- Full BlockHeader DB API (used for Service API)
--
blockHeaderDbServer
    :: CanReadablePayloadCas tbl
    => BlockHeaderDb_ v c
    -> PayloadDb tbl
    -> Server (BlockHeaderDbApi v c)
blockHeaderDbServer (BlockHeaderDb_ db) pdb
    = hashesHandler db
    :<|> headersHandler db defaultEntryLimit
    :<|> blocksHandler db pdb defaultEntryLimit
    :<|> headerHandler db
    :<|> branchHashesHandler db
    :<|> branchHeadersHandler db defaultBoundsLimit defaultEntryLimit
    :<|> branchBlocksHandler db pdb defaultBoundsLimit defaultEntryLimit

-- Restricted P2P BlockHeader DB API
--
p2pBlockHeaderDbServer :: BlockHeaderDb_ v c -> Server (P2pBlockHeaderDbApi v c)
p2pBlockHeaderDbServer (BlockHeaderDb_ db)
    = headersHandler db p2pEntryLimit
    :<|> headerHandler db
    :<|> branchHeadersHandler db p2pBoundsLimit p2pEntryLimit

-- -------------------------------------------------------------------------- --
-- Multichain Server

someBlockHeaderDbServer
    :: CanReadablePayloadCas tbl
    => SomeBlockHeaderDb
    -> PayloadDb tbl
    -> SomeServer
someBlockHeaderDbServer (SomeBlockHeaderDb (db :: BlockHeaderDb_ v c)) pdb
    = SomeServer (Proxy @(BlockHeaderDbApi v c)) (blockHeaderDbServer db pdb)

someBlockHeaderDbServers
    :: CanReadablePayloadCas tbl
    => ChainwebVersion
    -> [(ChainId, BlockHeaderDb)]
    -> [(ChainId, PayloadDb tbl)]
    -> SomeServer
someBlockHeaderDbServers v cdbs pdbs = mconcat
    [ someBlockHeaderDbServer (someBlockHeaderDbVal v cid cdb) pdb
    | (cid, (cdb, pdb)) <-
        Map.toList $ Map.intersectionWith (,) (Map.fromList cdbs) (Map.fromList pdbs)
    ]

someP2pBlockHeaderDbServer :: SomeBlockHeaderDb -> SomeServer
someP2pBlockHeaderDbServer (SomeBlockHeaderDb (db :: BlockHeaderDb_ v c))
    = SomeServer (Proxy @(P2pBlockHeaderDbApi v c)) (p2pBlockHeaderDbServer db)

someP2pBlockHeaderDbServers :: ChainwebVersion -> [(ChainId, BlockHeaderDb)] -> SomeServer
someP2pBlockHeaderDbServers v = mconcat
    . fmap (someP2pBlockHeaderDbServer . uncurry (someBlockHeaderDbVal v))

-- -------------------------------------------------------------------------- --
-- BlockHeader Event Stream

someBlockStreamServer :: CanReadablePayloadCas tbl => ChainwebVersion -> CutDb tbl -> SomeServer
someBlockStreamServer (FromSingChainwebVersion (SChainwebVersion :: Sing v)) cdb =
    SomeServer (Proxy @(BlockStreamApi v)) $
        blockStreamHandler cdb True :<|> blockStreamHandler cdb False

blockStreamHandler :: forall tbl. CanReadablePayloadCas tbl => CutDb tbl -> Bool -> Tagged Handler Application
blockStreamHandler db withPayloads = Tagged $ \req resp -> do
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
        Just x <- lookupPayloadWithHeight cas (Just $ view blockHeight bh) (view blockPayloadHash bh)
        pure $ HeaderUpdate
            { _huHeader = ObjectEncoded bh
            , _huPayloadWithOutputs =
                x <$ guard withPayloads
            , _huTxCount = length $ _payloadWithOutputsTransactions x
            , _huPowHash = decodeUtf8 . B16.encode . BS.reverse . fromShort . powHashBytes $ view blockPow bh
            , _huTarget = showTargetHex $ view blockTarget bh
            }

    f :: HeaderUpdate -> ServerEvent
    f hu = ServerEvent (Just $ fromByteString "BlockHeader") Nothing
        [ fromLazyByteString . encode $ toJSON hu ]
