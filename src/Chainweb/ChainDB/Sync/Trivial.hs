{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Chainweb.ChainDB.Sync.Trivial
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Chainweb.ChainDB.Sync.Trivial
( syncSession
) where

import Control.Concurrent
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.STM

import Data.Hashable
import qualified Data.HashSet as HS
import Data.Monoid.Unicode
import qualified Data.Text as T

import Prelude.Unicode

import System.Logger.LogLevel

-- internal modules

import P2P.Connection
import qualified Chainweb.ChainDB as DB

-- -------------------------------------------------------------------------- --
-- Sync Session

type Received = HS.HashSet (DB.Key 'DB.Unchecked)

syncSession
    ∷ ∀ m
    . MonadMask m
    ⇒ MonadAsync m
    ⇒ MonadIO m
    ⇒ (LogLevel → T.Text → m ())
    → DB.ChainDb
    → P2pConnection m
    → m ()
syncSession logg db co = go
    `catch` \case
        P2pConnectionClosed{} → logg Info "connection closed" >> p2pClose co
        P2pConnectionFailed{} → logg Info "connection failed" >> p2pClose co
  where
    go ∷ m ()
    go = do
        receivedVar ← liftIO $ newTVarIO mempty
        hashes ← liftIO $ DB.updates db
        runConcurrently_
            [ sendAllBlockHeaders receivedVar hashes
            , receiveBlockHeaders receivedVar
            ]

    sendAllBlockHeaders ∷ TVar Received → DB.Updates → m ()
    sendAllBlockHeaders receivedVar hashes = forever $ do
        s ← liftIO $ atomically $ DB.updatesNext hashes
        received ← liftIO $ HS.member (DB.uncheckedKey s) <$> readTVarIO receivedVar
        unless received $ do
            e ← liftIO $ do
                dbs ← DB.snapshot db
                DB.getEntryIO s dbs
            p2pSend co [DB.encodeEntry e]
            logg Debug $ "send block header " ⊕ showHash s

    receiveBlockHeaders ∷ TVar Received → m ()
    receiveBlockHeaders receivedVar = forever $ do
        msg ← p2pReceive co
        bh ← liftIO $ do
            bh ← DB.decodeEntry $ mconcat msg
            atomically $ modifyTVar receivedVar $! HS.insert (DB.key bh)
            dbs ← DB.snapshot db
            dbs' ← DB.insert bh dbs
            void $ DB.syncSnapshot dbs'
            return bh
        logg Debug $ "received block header " ⊕ showHash (DB.key bh)

-- -------------------------------------------------------------------------- --
-- Utils

showHash ∷ Hashable a ⇒ a → T.Text
showHash = T.pack ∘ show ∘ abs ∘ hash

runConcurrently_ ∷ MonadIO m ⇒ MonadMask m ⇒ MonadAsync m ⇒ [m a] → m ()
runConcurrently_ l = void $ mask $ \restore → do
    as ← mapM (async ∘ restore) l
    restore (waitAny (fst <$> as))
        `finally` mapM (liftIO ∘ uninterruptibleMask_ ∘ killThread) (snd <$> as)

waitAny ∷ MonadIO m ⇒ [TMVar a] → m a
waitAny vars = liftIO
    $ atomically $ foldr (orElse ∘ takeTMVar) retry vars

