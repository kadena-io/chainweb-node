{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: P2P.TaskQueue
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- TODO
--
module P2P.TaskQueue
(
-- * Tasks
  TaskAction
, TaskId(..)
, AttemptsCount(..)
, Priority(..)
, Task(..)
, newTask
, awaitTask
, taskResult

-- * P2P Session for Running Queued Tasks
, session
, session_

-- * Exceptions
, TaskException(..)
) where

import Control.Arrow
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Catch

import Data.Either
import Data.Function
import Data.Hashable
import Data.String
import qualified Data.Text as T

import GHC.Generics (Generic)

import Numeric.Natural

import System.LogLevel

-- internal modules

import Chainweb.Utils

import Data.IVar
import Data.LogMessage
import Data.PQueue

import P2P.Peer

-- -------------------------------------------------------------------------- --
-- Exceptions

data TaskException = TaskFailed [SomeException]
    deriving Show

instance Exception TaskException

-- -------------------------------------------------------------------------- --
-- Task

type TaskAction env a = LogFunction -> env -> IO a

newtype TaskId = TaskId T.Text
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (IsString)

newtype Priority = Priority Int
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData, Hashable)

newtype AttemptsCount = AttemptsCount Natural
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (NFData, Hashable)
    deriving newtype (Num, Enum, Real, Integral)

data Task env a = Task
    { _taskId :: !TaskId
    , _taskAction :: !(TaskAction env a)
    , _taskPriority :: !Priority
    , _taskAttemptsCount :: !AttemptsCount
    , _taskResult :: !(IVar (Either [SomeException] a))
    , _taskFailures :: ![SomeException]
    }

makeLensesFor
    [ ("_taskAttemptsCount", "taskAttemptsCount")
    , ("_taskFailures", "taskFailures")
    ]
    'Task

newTask :: TaskId -> Priority -> TaskAction env a -> IO (Task env a)
newTask tid prio act = do
    var <- newIVar
    return $ Task
        { _taskId = tid
        , _taskAction = act
        , _taskPriority = prio
        , _taskAttemptsCount = 0
        , _taskResult = var
        , _taskFailures = []
        }

awaitTask :: Task env a -> IO a
awaitTask = awaitIVar . _taskResult >=> either (throwM . TaskFailed) return

taskResult :: Task env a -> RIVar (Either [SomeException] a)
taskResult = rIVar . _taskResult
{-# INLINE taskResult #-}

instance Eq (Task env a) where
    (==) = (==) `on` (_taskPriority &&& _taskAttemptsCount)
    {-# INLINE (==) #-}

instance Ord (Task env a) where
    compare = compare `on` (_taskPriority &&& _taskAttemptsCount)
    {-# INLINE compare #-}

-- -------------------------------------------------------------------------- --
-- P2P Session Backend for Task Queue

-- | This session is intended to be used by a P2P node. It's type is general
-- enough to be useful in other contexts, too.
--
session
    :: AttemptsCount
    -> PQueue (Task env a)
    -> LogFunction
    -> env
    -> PeerInfo
    -> IO Bool
session li q lo e _ = session_ li q lo e
{-# INLINE session #-}

session_
    :: AttemptsCount
    -> PQueue (Task env a)
    -> LogFunction
    -> env
    -> IO Bool
session_ limit q logFun env = mask $ \restore -> do
    task <- pQueueRemove q

    -- check if the result variable as already been filled
    let go = tryReadIVar (_taskResult task) >>= \case
            Nothing -> do
                logg task Debug "run task"
                flip catchAllSynchronous (retry task) $ restore $ do
                    r <- _taskAction task logFun env
                    putResult (_taskResult task) $! Right r
            Just Left{} -> do
                logg task Debug "task already failed"
                return False
            Just Right{} -> do
                logg task Debug "task already succeeded"
                return True

    go `catch` \(e :: SomeException) -> do
        void $ retry task e
        throwM e

  where
    -- reschedule a task or fail if maximum number of attempts has been reached.
    --
    retry task e = do
        let !task' = task
                & over taskAttemptsCount succ
                & over taskFailures ((:) e)
        let attempts = _taskAttemptsCount task'
        if
            | _taskAttemptsCount task' < limit -> do
                logg task' Info $ "task failed: " <> sshow attempts
                pQueueInsert q task'
                return False
            | otherwise -> do
                logg task' Warn
                    $ "task finally failed: " <> sshow attempts
                    <> ", limit: " <> sshow limit
                putResult (_taskResult task') $! Left $! _taskFailures task'

    logg task l m = logFun @T.Text l $ sshow (_taskId task) <> ": " <> m

    -- Put result. If the variable is already filled, derive the return value on
    -- the existing result.
    --
    putResult var result = putIVar var result >>= \case
        True -> return True
        False -> isRight <$> awaitIVar var
            -- note that the var won't change it's value once it is filled

