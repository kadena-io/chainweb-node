{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Module: TXG.Types
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--
-- TODO
--
module TXG.Types
  ( -- * TXCmd
    TXCmd(..)
    -- * Timing
  , TimingDistribution(..)
    -- * Args
  , Args(..)
  , defaultArgs
  , scriptConfigParser
    -- * TXG Monad
  , TXG(..)
  , TXGState(..)
  , TXGConfig(..), mkTXGConfig
    -- * Misc.
  , TXCount(..)
  , BatchSize(..)
  ) where

import BasePrelude hiding (loop, option, rotate, timeout, (%))
import Chainweb.ChainId
import Chainweb.HostAddress
import Chainweb.Utils (HasTextRepresentation(..), fromJuste, textOption)
import Chainweb.Version
import Configuration.Utils hiding (Error, Lens', (<.>))
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Primitive
import Control.Monad.Reader hiding (local)
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import Data.Generics.Product.Fields (field)
import Data.Map (Map)
import Data.Sequence.NonEmpty (NESeq(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client hiding (Proxy, host)
import Network.HTTP.Client.TLS
import Network.X509.SelfSigned hiding (name)
import Pact.Types.Crypto (SomeKeyPair)
import Servant.Client
import System.Random.MWC (Gen)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified TXG.Simulate.Contracts.Common as Sim
import qualified Utils.Logging.Config as U

---

data TimingDistribution
  = Gaussian { mean  :: !Double, var   :: !Double }
  | Uniform  { low   :: !Double, high  :: !Double }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

defaultTimingDist :: TimingDistribution
defaultTimingDist = Gaussian 1000000 (1000000 / 16)

data TXCmd
  = DeployContracts [Sim.ContractName]
  | RunStandardContracts TimingDistribution
  | RunSimpleExpressions TimingDistribution
  | PollRequestKeys ByteString
  | ListenerRequestKey ByteString
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

transactionCommandToText :: TXCmd -> Text
transactionCommandToText = T.decodeUtf8 . fromJuste . transactionCommandBytes
{-# INLINE transactionCommandToText #-}

transactionCommandBytes :: TXCmd -> Maybe B8.ByteString
transactionCommandBytes t = case t of
  PollRequestKeys bs -> Just $ "poll [" <> bs <> "]"
  ListenerRequestKey bs -> Just $ "listen " <> bs
  _ -> Nothing

transactionCommandFromText :: MonadThrow m => Text -> m TXCmd
transactionCommandFromText = readTXCmdBytes . T.encodeUtf8
{-# INLINE transactionCommandFromText #-}

readTXCmdBytes :: MonadThrow m => B8.ByteString -> m TXCmd
readTXCmdBytes = Sim.parseBytes "transaction-command" transactionCommandParser
{-# INLINE readTXCmdBytes #-}

transactionCommandParser :: A.Parser TXCmd
transactionCommandParser = pollkeys <|> listenkeys

pollkeys :: A.Parser TXCmd
pollkeys = do
  _constructor <- A.string "poll"
  A.skipSpace
  _open <- A.char '[' >> A.skipSpace
  bs <- parseRequestKey
  _close <- A.skipSpace >> A.char ']'
  pure $ PollRequestKeys bs

parseRequestKey :: A.Parser ByteString
parseRequestKey = B8.pack <$> A.count 128 (A.satisfy (A.inClass "abcdef0123456789"))

listenkeys :: A.Parser TXCmd
listenkeys = do
  _constructor <- A.string "listen"
  A.skipSpace
  bytestring <- parseRequestKey
  pure $ ListenerRequestKey bytestring

instance HasTextRepresentation TXCmd where
  toText = transactionCommandToText
  {-# INLINE toText #-}
  fromText = transactionCommandFromText
  {-# INLINE fromText #-}

-------
-- Args
-------

data Args = Args
  { scriptCommand :: !TXCmd
  , nodeChainIds :: ![ChainId]
  , isChainweb :: !Bool
  , hostAddresses :: ![HostAddress]
  , nodeVersion :: !ChainwebVersion
  , logHandleConfig :: !U.HandleConfig
  , batchSize :: !BatchSize
  } deriving (Show, Generic)

instance ToJSON Args where
  toJSON o = object
    [ "scriptCommand"   .= scriptCommand o
    , "nodeChainIds"    .= nodeChainIds o
    , "isChainweb"      .= isChainweb o
    , "hostAddresses"   .= hostAddresses o
    , "chainwebVersion" .= nodeVersion o
    , "logHandle"       .= logHandleConfig o
    , "batchSize"       .= batchSize o
    ]

instance FromJSON (Args -> Args) where
  parseJSON = withObject "Args" $ \o -> id
    <$< field @"scriptCommand"   ..: "scriptCommand"   % o
    <*< field @"nodeChainIds"    ..: "nodeChainIds"    % o
    <*< field @"isChainweb"      ..: "isChainweb"      % o
    <*< field @"hostAddresses"   ..: "hostAddresses"   % o
    <*< field @"nodeVersion"     ..: "chainwebVersion" % o
    <*< field @"logHandleConfig" ..: "logging"         % o
    <*< field @"batchSize"       ..: "batchSize"       % o

defaultArgs :: Args
defaultArgs = Args
  { scriptCommand   = RunSimpleExpressions defaultTimingDist
  , nodeChainIds    = []
  , isChainweb      = True
  , hostAddresses   = [unsafeHostAddressFromText "127.0.0.1:1789"]
  , nodeVersion     = v
  , logHandleConfig = U.StdOut
  , batchSize       = BatchSize 1 }
  where
    v :: ChainwebVersion
    v = fromJuste $ chainwebVersionFromText "timedCPM-peterson"

scriptConfigParser :: MParser Args
scriptConfigParser = id
  <$< field @"scriptCommand" .:: textOption
      % long "script-command"
      <> short 'c'
      <> metavar "COMMAND"
      <> help ("The specific command to run: see examples/transaction-generator-help.md for more detail."
               <> "The only commands supported on the commandline are 'poll' and 'listen'.")
  <*< field @"nodeChainIds" %:: pLeftSemigroupalUpdate (pure <$> pChainId)
  <*< field @"hostAddresses" %:: pLeftSemigroupalUpdate (pure <$> pHostAddress' Nothing)
  <*< field @"nodeVersion" .:: textOption
      % long "chainweb-version"
      <> short 'v'
      <> metavar "VERSION"
      <> help "Chainweb Version"
  <*< field @"batchSize" .:: option auto
      % long "batch-size"
      <> short 'b'
      <> metavar "COUNT"
      <> help "Number of transactions to bundle into a single 'send' call"
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
  { gsGen :: !(Gen (PrimState IO))
  , gsCounter :: !(TVar TXCount)
  , gsChains :: !(NESeq ChainId)
  } deriving (Generic)

data TXGConfig = TXGConfig
  { confTimingDist :: !(Maybe TimingDistribution)
  , confKeysets :: !(Map ChainId (Map Sim.Account (Map Sim.ContractName [SomeKeyPair])))
  , confClientEnv :: !ClientEnv
  , confVersion :: !ChainwebVersion
  , confBatchSize :: !BatchSize
  } deriving (Generic)

mkTXGConfig :: Maybe TimingDistribution -> Args -> HostAddress -> IO TXGConfig
mkTXGConfig mdistribution config host =
  TXGConfig mdistribution mempty
  <$> cenv
  <*> pure (nodeVersion config)
  <*> pure (batchSize config)
  where
    cenv :: IO ClientEnv
    cenv = do
       mgrSettings <- certificateCacheManagerSettings TlsInsecure Nothing
       let timeout = responseTimeoutMicro (1000000 * 60 * 4)
       mgr <- newTlsManagerWith (mgrSettings { managerResponseTimeout = timeout })
       let url = BaseUrl Https
                 (T.unpack . hostnameToText $ _hostAddressHost host)
                 (fromIntegral $ _hostAddressPort host)
                 ""
       pure $! mkClientEnv mgr url

-------
-- MISC
-------

-- | A running count of all transactions handles over all threads.
newtype TXCount = TXCount Word
  deriving newtype (Num, Show)

newtype BatchSize = BatchSize Word
  deriving newtype (Integral, Real, Num, Enum, Ord, Eq, Read, Show, ToJSON, FromJSON)
