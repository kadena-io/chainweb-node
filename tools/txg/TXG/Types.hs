{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Module: TXG.Types
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--
-- TODO
--
module TXG.Types
  ( -- * TransactionCommand
    TransactionCommand(..)
    -- * Timing
  , MeasureTime(..)
  , TimingDistribution(..)
    -- * ScriptConfig
  , ScriptConfig(..)
  , defaultScriptConfig
  , scriptConfigParser
    -- * TXG Monad
  , TXG(..)
  , TXGConfig(..), confKeysets
  , TXGState(..), gsChains
  ) where

import BasePrelude hiding (loop, rotate, timeout, (%))
import Chainweb.ChainId
import Chainweb.HostAddress
import Chainweb.Utils (HasTextRepresentation(..), fromJuste, textOption)
import Chainweb.Version
import Configuration.Utils hiding (Error, Lens', (<.>))
import Control.Lens hiding (op, (.=), (|>))
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Primitive
import Control.Monad.Reader hiding (local)
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import Data.Default
import Data.Map (Map)
import Data.Sequence.NonEmpty (NESeq(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Pact.Types.Crypto (SomeKeyPair)
import Servant.Client
import System.Random.MWC (Gen)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B8
import qualified Data.Queue.Bounded as BQ
import qualified Data.Text.Encoding as T
import qualified TXG.Simulate.Contracts.Common as Sim
import qualified Utils.Logging.Config as U

---

newtype MeasureTime = MeasureTime { measureTime :: Bool }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Default MeasureTime where
  def = MeasureTime False

data TimingDistribution
  = Gaussian { mean  :: !Double, var   :: !Double }
  | Uniform  { low   :: !Double, high  :: !Double }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Default TimingDistribution where
  def = Gaussian 1000000 (1000000 / 16)

data TransactionCommand
  = DeployContracts [Sim.ContractName] MeasureTime
  | RunStandardContracts TimingDistribution MeasureTime
  | RunSimpleExpressions TimingDistribution MeasureTime
  | PollRequestKeys ByteString MeasureTime
  | ListenerRequestKey ByteString MeasureTime
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

transactionCommandToText :: TransactionCommand -> Text
transactionCommandToText = T.decodeUtf8 . fromJuste . transactionCommandBytes
{-# INLINE transactionCommandToText #-}

transactionCommandBytes :: TransactionCommand -> Maybe B8.ByteString
transactionCommandBytes t = case t of
  PollRequestKeys bs (MeasureTime mtime) ->
    Just $ "poll [" <> bs <> "] " <> (fromString . map toLower . show $ mtime)
  ListenerRequestKey bs (MeasureTime mtime) ->
    Just $ "listen " <> bs <> " " <> (fromString . map toLower . show $ mtime)
  _ -> Nothing

transactionCommandFromText :: MonadThrow m => Text -> m TransactionCommand
transactionCommandFromText = readTransactionCommandBytes . T.encodeUtf8
{-# INLINE transactionCommandFromText #-}

readTransactionCommandBytes :: MonadThrow m => B8.ByteString -> m TransactionCommand
readTransactionCommandBytes = Sim.parseBytes "transaction-command" transactionCommandParser
{-# INLINE readTransactionCommandBytes #-}

transactionCommandParser :: A.Parser TransactionCommand
transactionCommandParser = pollkeys <|> listenkeys

pollkeys :: A.Parser TransactionCommand
pollkeys = do
  _constructor <- A.string "poll"
  A.skipSpace
  _open <- A.char '[' >> A.skipSpace
  bs <- parseRequestKey
  _close <- A.skipSpace >> A.char ']'
  A.skipSpace
  measure <- MeasureTime <$> ((False <$ A.string "false") <|> (True <$ A.string "true"))
  pure $ PollRequestKeys bs measure

parseRequestKey :: A.Parser ByteString
parseRequestKey = B8.pack <$> A.count 128 (A.satisfy (A.inClass "abcdef0123456789"))

listenkeys :: A.Parser TransactionCommand
listenkeys = do
  _constructor <- A.string "listen"
  A.skipSpace
  bytestring <- parseRequestKey
  A.skipSpace
  measure <- MeasureTime <$> ((False <$ A.string "false") <|> (True <$ A.string "true"))
  pure $ ListenerRequestKey bytestring measure

instance HasTextRepresentation TransactionCommand where
  toText = transactionCommandToText
  {-# INLINE toText #-}
  fromText = transactionCommandFromText
  {-# INLINE fromText #-}

instance Default TransactionCommand where
  def = RunSimpleExpressions def def

---------------
-- ScriptConfig
---------------

data ScriptConfig = ScriptConfig
  { _scriptCommand :: !TransactionCommand
  , _nodeChainIds :: ![ChainId]
  , _isChainweb :: !Bool
  , _hostAddresses :: ![HostAddress]
  , _nodeVersion :: !ChainwebVersion
  , _logHandleConfig :: !U.HandleConfig }
  deriving (Show, Generic)

makeLenses ''ScriptConfig

instance ToJSON ScriptConfig where
  toJSON o =
    object
      [ "scriptCommand"       .= _scriptCommand o
      , "nodeChainIds"        .= _nodeChainIds o
      , "isChainweb"          .= _isChainweb o
      , "hostAddresses"       .= _hostAddresses o
      , "chainwebVersion"     .= _nodeVersion o
      , "logHandle"           .= _logHandleConfig o
      ]

instance FromJSON (ScriptConfig -> ScriptConfig) where
  parseJSON = withObject "ScriptConfig" $ \o -> id
    <$< scriptCommand       ..: "scriptCommand"       % o
    <*< nodeChainIds        ..: "nodeChainIds"        % o
    <*< isChainweb          ..: "isChainweb"          % o
    <*< hostAddresses       ..: "hostAddresses"       % o
    <*< nodeVersion         ..: "chainwebVersion"     % o
    <*< logHandleConfig     ..: "logging"             % o

defaultScriptConfig :: ScriptConfig
defaultScriptConfig = ScriptConfig
  { _scriptCommand   = RunSimpleExpressions def def
  , _nodeChainIds    = []
  , _isChainweb      = True
  , _hostAddresses   = [unsafeHostAddressFromText "127.0.0.1:1789"]
  , _nodeVersion     = v
  , _logHandleConfig = U.StdOut }
  where
    v :: ChainwebVersion
    v = fromJuste $ chainwebVersionFromText "timedCPM-peterson"

scriptConfigParser :: MParser ScriptConfig
scriptConfigParser = id
  <$< scriptCommand .:: textOption
      % long "script-command"
      <> short 'c'
      <> metavar "COMMAND"
      <> help ("The specific command to run: see examples/transaction-generator-help.md for more detail."
               <> "The only commands supported on the commandline are 'poll' and 'listen'.")
  <*< nodeChainIds %:: pLeftSemigroupalUpdate (pure <$> pChainId)
  <*< hostAddresses %:: pLeftSemigroupalUpdate (pure <$> pHostAddress' Nothing)
  <*< nodeVersion .:: textOption
      % long "chainweb-version"
      <> short 'v'
      <> metavar "VERSION"
      <> help "Chainweb Version"
  where
    pChainId = textOption
      % long "node-chain-id"
      <> short 'i'
      <> metavar "INT"
      <> help "The specific chain that will receive generated transactions. Can be used multiple times."

------------
-- TXG Monad
------------

-- TODO: Ideally we'd shove `LoggerT` into this stack, but `yet-another-logger`
-- would have to be patched to add missing instances first. Having `LoggerT`
-- here would let us remove the `MonadTrans` instance, as well as a number of
-- `lift` calls.

-- | The principal application Monad for this Transaction Generator.
newtype TXG m a = TXG { runTXG :: ReaderT TXGConfig (StateT TXGState m) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState TXGState, MonadReader TXGConfig)

instance MonadTrans TXG where
  lift = TXG . lift . lift

data TXGState = TXGState
  { _gsGen :: !(Gen (PrimState IO))
  , _gsCounter :: !(TVar Int64)
  , _gsChains :: !(NESeq ChainId)
  , _gsRespTimes :: !(TVar (BQ.BQueue Int))
  }

gsChains :: Lens' TXGState (NESeq ChainId)
gsChains f s = (\c -> s { _gsChains = c }) <$> f (_gsChains s)

data TXGConfig = TXGConfig
  { _confTimingDist :: !(Maybe TimingDistribution)
  , _confKeysets :: !(Map ChainId (Map Sim.Account (Map Sim.ContractName [SomeKeyPair])))
  , _confClientEnv :: !ClientEnv
  , _confVersion :: !ChainwebVersion
  }

confKeysets :: Lens' TXGConfig (Map ChainId (Map Sim.Account (Map Sim.ContractName [SomeKeyPair])))
confKeysets f c = (\ks -> c { _confKeysets = ks }) <$> f (_confKeysets c)
