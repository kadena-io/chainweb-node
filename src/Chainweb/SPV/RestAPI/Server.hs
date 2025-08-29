{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

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
( spvGetTransactionOutputProofHandler
, spvServer
, spvApp
, spvApiLayout
, someSpvServer
, someSpvServers
) where

import Control.Lens ((^?!))
import Control.Monad.IO.Class
import Control.Exception.Safe (try, Exception(..))
import Data.Foldable
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.CutDB
import Chainweb.PayloadProvider
import Chainweb.RestAPI.Utils
import Chainweb.SPV
import Chainweb.SPV.CreateProof
import Chainweb.SPV.RestAPI
import Chainweb.Utils
import Chainweb.Version
import Control.Lens (view)
import Data.Singletons
import Data.Text.IO qualified as T
import Numeric.Natural
import Servant
import qualified Data.ByteString.Lazy.Char8 as BL8
import Chainweb.MerkleUniverse

-- -------------------------------------------------------------------------- --
-- SPV Transaction Output Proof Handler

spvGetTransactionOutputProofHandler
    :: HasVersion
    => CutDb
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
    -> Handler (TransactionOutputProof ChainwebMerkleHashAlgorithm)
spvGetTransactionOutputProofHandler db tcid scid bh i =
    liftIO $ createTransactionOutputProof db tcid scid bh (int i)
    -- FIXME: add proper error handling

-- -------------------------------------------------------------------------- --
-- SPV Event Output Proof Handler

-- | TODO: we are probably missusing http status codes here. All
-- PayloadSPvExceptions represent Application level protocol failures and not
-- HTTP failures.
--
-- On the other hand, adding another level of failure reporting might be
-- overkill.
--
spvGetEventProofHandler
    :: HasVersion
    => CutDb
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
spvGetEventProofHandler db tcid scid bh i e = do
    (try getProof) >>= \case
        Left err -> do
            let msg = BL8.pack (displayException err)
            throwError $ case err of
                InvalidBlockHeight {} -> err404 { errBody = msg }
                InvalidTransactionIndex {} -> err404 { errBody = msg }
                InvalidEventIndex {} -> err404 { errBody = msg }
                UnsupportedEventType {} -> err422 { errBody = msg }
                InvalidEvent {} -> err422 { errBody = msg }
                    -- or err400?
                ProofPending {} -> err404 { errBody = msg }
        Right v ->
            return $ FakeEventProof tcid v
  where
    getProof = case providers ^?! atChain scid of
        ConfiguredPayloadProvider p -> do
            -- trgHeader <- minimumTrgHeader headerDb tcid scid bh
            -- TODO: check through block height whether a proof can possibly
            -- already by available before we do any expensive computations.
            (SpvProof v) <- liftIO $ eventProof p xevent
            return v
        DisabledPayloadProvider ->
            throwError err404 { errBody = BL8.pack "disabled payload provider on chain" }

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
    => HasVersion
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
    => HasVersion
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
    => HasVersion
    => SomeCutDb
    -> SomeServer
someSpvServer (SomeCutDb (db :: CutDbT v))
    = SomeServer (Proxy @(SpvApi v c)) (spvServer @v @c db)

-- -------------------------------------------------------------------------- --
-- Multichain Server

someSpvServers
    :: HasVersion
    => CutDb
    -> SomeServer
someSpvServers db = mconcat $ flip fmap cids $ \(FromSingChainId (SChainId :: Sing c)) ->
    someSpvServer @c (someCutDbVal db)
  where
    cids = toList chainIds
