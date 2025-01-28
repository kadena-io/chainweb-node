{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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

import Numeric.Natural

import Servant

-- internal modules

import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.CutDB
import Chainweb.RestAPI.Utils
import Chainweb.SPV
import Chainweb.SPV.CreateProof
import Chainweb.SPV.RestAPI
import Chainweb.Utils
import Chainweb.Version

import Data.Singletons
import Chainweb.PayloadProvider
import Control.Lens (view)

-- -------------------------------------------------------------------------- --
-- SPV Transaction Proof Handler

spvGetTransactionProofHandler
    :: CutDb
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
    -> Handler (TransactionProof SHA512t_256)
spvGetTransactionProofHandler db tcid scid bh i =
    liftIO $ createTransactionProof db tcid scid bh (int i)
    -- FIXME: add proper error handling

-- -------------------------------------------------------------------------- --
-- SPV Transaction Output Proof Handler

spvGetTransactionOutputProofHandler
    :: CutDb
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
    -> Handler (TransactionOutputProof SHA512t_256)
spvGetTransactionOutputProofHandler db tcid scid bh i =
    liftIO $ createTransactionOutputProof db tcid scid bh (int i)
    -- FIXME: add proper error handling

-- -------------------------------------------------------------------------- --
-- SPV Event Output Proof Handler

spvGetEventProofHandler
    :: CutDb
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
    -> Natural
        -- ^ The event index in the transaction
    -> Handler FakeEventProof
spvGetEventProofHandler db tcid scid bh i e = liftIO $ do
    withPayloadProvider providers scid $ \p -> do
        (SpvProof v) <- liftIO $ eventProof p xevent
        return $ FakeEventProof tcid v
  where
    providers = view cutDbPayloadProviders db
    xevent = XEventId
        { _xEventBlockHeight = bh
        , _xEventTransactionIndex = int i
        , _xEventEventIndex = int e
        }

-- -------------------------------------------------------------------------- --
-- SPV API Server

spvServer
    :: forall v (c :: ChainIdT)
    . KnownChainIdSymbol c
    => CutDbT v
    -> Server (SpvApi v c)
spvServer (CutDbT db)
    = spvGetEventProofHandler db tcid
    -- = spvGetTransactionProofHandler db tcid
    -- :<|> spvGetTransactionOutputProofHandler db tcid
  where
    tcid = fromSing (sing :: Sing c)

-- -------------------------------------------------------------------------- --
-- Application for a single Chain

spvApp
    :: forall v c
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => CutDbT v
    -> Application
spvApp db = serve (Proxy @(SpvApi v c)) (spvServer @v @c db)

spvApiLayout
    :: forall v c
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => CutDbT v
    -> IO ()
spvApiLayout _ = T.putStrLn $ layout (Proxy @(SpvApi v c))

someSpvServer
    :: forall c
    . KnownChainIdSymbol c
    => SomeCutDb
    -> SomeServer
someSpvServer (SomeCutDb (db :: CutDbT v))
    = SomeServer (Proxy @(SpvApi v c)) (spvServer @v @c db)

-- -------------------------------------------------------------------------- --
-- Multichain Server

someSpvServers
    :: ChainwebVersion
    -> CutDb
    -> SomeServer
someSpvServers v db = mconcat $ flip fmap cids $ \(FromSingChainId (SChainId :: Sing c)) ->
    someSpvServer @c (someCutDbVal v db)
  where
    cids = toList $ chainIds db
