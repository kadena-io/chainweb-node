{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: System.Logger
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Chainweb Logger and logging tools.
--
module Chainweb.Logger
( Logger
, logFunction
, logFunctionText
, logFunctionJson

-- * Utils
, addLabel
, setComponent
, l2l
, l2l'

-- * Generic Test Logger
, GenericLogger
, genericLogger
) where

import Control.Lens
import qualified Data.Text as T
import Data.Time

import qualified System.Logger.Types as L

-- internal modules

import Chainweb.Utils

import Data.LogMessage

import System.LogLevel

-- -------------------------------------------------------------------------- --
-- Abstract Logger

type Logger l = L.LoggerCtx l SomeLogMessage

-- -------------------------------------------------------------------------- --
-- Utils

logFunction :: Logger l => l -> LogFunction
logFunction logger level = L.loggerFunIO logger (l2l level) . toLogMessage
{-# INLINE logFunction #-}

logFunctionText :: Logger l => l -> LogFunctionText
logFunctionText logger level = logFunction logger level . TextLog
{-# INLINE logFunctionText #-}

logFunctionJson
    :: Logger l
    => l
    -> LogFunctionJson a
logFunctionJson logger level = logFunction logger level . JsonLog
{-# INLINE logFunctionJson #-}

-- -------------------------------------------------------------------------- --
-- Utils

setComponent :: Logger logger => T.Text -> logger -> logger
setComponent c = addLabel ("component", c)
{-# INLINE setComponent #-}

addLabel :: Logger logger => (T.Text, T.Text) -> logger -> logger
addLabel = over L.setLoggerScope . (:)
{-# INLINE addLabel #-}

l2l :: LogLevel -> L.LogLevel
l2l Quiet = L.Quiet
l2l Error = L.Error
l2l Warn = L.Warn
l2l Info = L.Info
l2l Debug = L.Debug
l2l (Other _) = L.Debug
{-# INLINE l2l #-}

l2l' :: L.LogLevel -> LogLevel
l2l' L.Quiet = Quiet
l2l' L.Error = Error
l2l' L.Warn = Warn
l2l' L.Info = Info
l2l' L.Debug = Debug
{-# INLINE l2l' #-}

-- -------------------------------------------------------------------------- --
-- Generic Test Logger

data GenericLogger = GenericLogger
    { _glScope :: ![(T.Text, T.Text)]
    , _glLevel :: !LogLevel
    , _glPolicy :: !L.LogPolicy
    , _glFun :: !(T.Text -> IO ())
    }

makeLenses 'GenericLogger

instance L.LoggerCtx GenericLogger SomeLogMessage where
    loggerFunIO ctx level msg
        | level <= l2l (_glLevel ctx) = do
            now <- getCurrentTime
            view glFun ctx
                $ sq (sshow now)
                <> sq (sshow level)
                <> sq (scope $ _glScope ctx)
                <> " "
                <> logText msg
        | otherwise = return ()
      where
        scope l = T.intercalate "," $ (\(a,b) -> (a <> ":" <> b)) <$> reverse l
        sq t = "[" <> t <> "]"

    setLoggerLevel = glLevel . iso l2l l2l'
    setLoggerScope = glScope
    setLoggerPolicy = glPolicy

    {-# INLINABLE loggerFunIO #-}
    {-# INLINE setLoggerLevel #-}
    {-# INLINE setLoggerScope #-}
    {-# INLINE setLoggerPolicy #-}

-- | Simpel generic logger for chainweb. For production purposes a proper
-- logging framework should be used.
--
genericLogger :: LogLevel -> (T.Text -> IO ()) -> GenericLogger
genericLogger level fun = GenericLogger [] level L.LogPolicyBlock fun
