{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Chainweb.SPV.RestAPI.Server
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Server implementation of the SPV REST API
--
module Chainweb.SPV.RestAPI.Server
( spvGetTransactionProofHandler
, spvGetTransactionOutputProofHandler
, newSpvServer
, spvServer
, spvApp
, spvApiLayout
, someSpvServer
, someSpvServers
) where

import Control.Monad.IO.Class

import Crypto.Hash.Algorithms

import Data.Foldable
import qualified Data.Text.IO as T

import Network.HTTP.Types
import Network.Wai
import Numeric.Natural

import Servant

import Web.DeepRoute
import Web.DeepRoute.Wai

-- internal modules

import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.CutDB
import Chainweb.Payload.PayloadStore
import Chainweb.RestAPI.Utils
import Chainweb.SPV
import Chainweb.SPV.CreateProof
import Chainweb.SPV.RestAPI
import Chainweb.Utils
import Chainweb.Version

import Data.Singletons

-- -------------------------------------------------------------------------- --
-- SPV Transaction Proof Handler

spvGetTransactionProofHandler
    :: (CanReadablePayloadCas tbl, MonadIO m)
    => CutDb tbl
    -> ChainId
        -- ^ the target chain of the proof. This is the chain for which
        -- inclusion is proved.
    -> ChainId
        -- ^ the source chain of the proof. This is the chain where the proof
        -- subject, the transaction for which inclusion is proven, is located.
    -> BlockHeight
        -- ^ the block height of the proof subject, the transaction for which
        -- inclusion is proven.
    -> Natural
        -- ^ the index of the proof subject, the transaction for which inclusion
        -- is proven.
    -> m (TransactionProof SHA512t_256)
spvGetTransactionProofHandler db tcid scid bh i =
    liftIO $ createTransactionProof db tcid scid bh (int i)
    -- FIXME: add proper error handling

-- -------------------------------------------------------------------------- --
-- SPV Transaction Output Proof Handler

spvGetTransactionOutputProofHandler
    :: (CanReadablePayloadCas tbl, MonadIO m)
    => CutDb tbl
    -> ChainId
        -- ^ the target chain of the proof. This is the chain for which inclusion
        -- is proved.
    -> ChainId
        -- ^ the source chain of the proof. This is the chain where the proof
        -- subject, the transaction  output for which inclusion is proven, is
        -- located.
    -> BlockHeight
        -- ^ the block height of the proof subject, the transaction output for
        -- which inclusion is proven.
    -> Natural
        -- ^ the index of the proof subject, the transaction output for which
        -- inclusion is proven.
    -> m (TransactionOutputProof SHA512t_256)
spvGetTransactionOutputProofHandler db tcid scid bh i =
    liftIO $ createTransactionOutputProof db tcid scid bh (int i)
    -- FIXME: add proper error handling

-- -------------------------------------------------------------------------- --
-- SPV API Server

spvServer
    :: forall tbl v (c :: ChainIdT)
    . CanReadablePayloadCas tbl
    => KnownChainIdSymbol c
    => CutDbT tbl v
    -> Server (SpvApi v c)
spvServer (CutDbT db)
    = spvGetTransactionProofHandler db tcid
    :<|> spvGetTransactionOutputProofHandler db tcid
  where
    tcid = fromSing (sing :: Sing c)

newSpvServer :: CanReadablePayloadCas tbl => CutDb tbl -> Route (ChainId -> ChainId -> Application)
newSpvServer cutDb =
    seg "height" $ capture $ fold
        [ seg "transaction" $ capture $
            endpoint methodGet ("application/json") $ \txIdx blockHeight spvChainId srcChainId _ resp ->
                resp . responseJSON ok200 [] =<< spvGetTransactionProofHandler cutDb srcChainId spvChainId blockHeight txIdx
        , seg "output" $ capture $
            endpoint methodGet ("application/json") $ \txOutputIdx blockHeight spvChainId srcChainId _ resp ->
                resp . responseJSON ok200 [] =<< spvGetTransactionOutputProofHandler cutDb srcChainId spvChainId blockHeight txOutputIdx
        ]

-- -------------------------------------------------------------------------- --
-- Application for a single Chain

spvApp
    :: forall tbl v c
    . CanReadablePayloadCas tbl
    => KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => CutDbT tbl v
    -> Application
spvApp db = serve (Proxy @(SpvApi v c)) (spvServer @tbl @v @c db)

spvApiLayout
    :: forall tbl v c
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => CutDbT tbl v
    -> IO ()
spvApiLayout _ = T.putStrLn $ layout (Proxy @(SpvApi v c))

someSpvServer
    :: forall tbl c
    . CanReadablePayloadCas tbl
    => KnownChainIdSymbol c
    => SomeCutDb tbl
    -> SomeServer
someSpvServer (SomeCutDb (db :: CutDbT tbl v))
    = SomeServer (Proxy @(SpvApi v c)) (spvServer @tbl @v @c db)

-- -------------------------------------------------------------------------- --
-- Multichain Server

someSpvServers
    :: CanReadablePayloadCas tbl
    => ChainwebVersion
    -> CutDb tbl
    -> SomeServer
someSpvServers v db = mconcat $ flip fmap cids $ \(FromSingChainId (SChainId :: Sing c)) ->
    someSpvServer @_ @c (someCutDbVal v db)
  where
    cids = toList $ chainIds db
