{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
-- TODO
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
    :: PayloadCas cas
    => CutDb cas
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
    :: PayloadCas cas
    => CutDb cas
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
-- SPV API Server

spvServer
    :: forall cas v (c :: ChainIdT)
    . PayloadCas cas
    => KnownChainIdSymbol c
    => CutDbT cas v
    -> Server (SpvApi v c)
spvServer (CutDbT db)
    = (spvGetTransactionProofHandler db tcid)
    :<|> (spvGetTransactionOutputProofHandler db tcid)
  where
    tcid = fromSing (sing :: Sing c)

-- -------------------------------------------------------------------------- --
-- Application for a single Chain

spvApp
    :: forall cas v c
    . PayloadCas cas
    => KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => CutDbT cas v
    -> Application
spvApp db = serve (Proxy @(SpvApi v c)) (spvServer @cas @v @c db)

spvApiLayout
    :: forall cas v c
    . KnownChainwebVersionSymbol v
    => KnownChainIdSymbol c
    => CutDbT cas v
    -> IO ()
spvApiLayout _ = T.putStrLn $ layout (Proxy @(SpvApi v c))

someSpvServer
    :: forall cas c
    . PayloadCas cas
    => KnownChainIdSymbol c
    => SomeCutDb cas
    -> SomeServer
someSpvServer (SomeCutDb (db :: CutDbT cas v))
    = SomeServer (Proxy @(SpvApi v c)) (spvServer @cas @v @c db)

-- -------------------------------------------------------------------------- --
-- Multichain Server

someSpvServers
    :: PayloadCas cas
    => ChainwebVersion
    -> CutDb cas
    -> SomeServer
someSpvServers v db = mconcat $ flip fmap cids $ \(FromSing (SChainId :: Sing c)) ->
    someSpvServer @_ @c (someCutDbVal v db)
  where
    cids = toList $ chainIds db
