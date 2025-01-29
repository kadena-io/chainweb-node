{-# language
    BangPatterns
    , DerivingStrategies
    , GeneralizedNewtypeDeriving
    , ImportQualifiedPost
    , LambdaCase
    , StandaloneDeriving
#-}

{-# options_ghc -fno-warn-orphans #-}

module Chainweb.Utils.Bench
  ( NoopNFData(..)
  , testLogger
  ) where

import Chainweb.Logger
import Pact.Core.Errors
import Chainweb.Test.Cut.TestBlockDb (TestBlockDb)
import Chainweb.Test.Pact5.Utils (getTestLogLevel)
import Chainweb.Test.Utils ()
import Database.SQLite3.Direct (Database(..))
import Chainweb.WebBlockHeaderDB (WebBlockHeaderDb)
import Chainweb.Pact.Types (PactServiceEnv)
import Control.DeepSeq (NFData(..))
import Chainweb.Mempool.Mempool (MempoolBackend)
import Chainweb.Pact.Service.PactQueue (PactQueue)
import Control.Monad.IO.Class (liftIO)
import Data.Text.IO qualified as Text

-- | Create a 'GenericLogger' by inspecting the
--   'CHAINWEB_TEST_LOG_LEVEL' environment variable.
testLogger :: IO GenericLogger
testLogger = do
    logLevel <- liftIO getTestLogLevel
    pure $ genericLogger logLevel Text.putStrLn

-- | Newtype to provide a noop 'NFData' instance.
--
--   Intended for use in criterion's 'envWithCleanup'
--   which wants environment values to be 'NFData'.
newtype NoopNFData a = NoopNFData a
  deriving stock (Show)

instance NFData (NoopNFData a) where
  rnf _ = ()

-- Orphan Instances --

deriving newtype instance NFData Database

instance NFData (PactServiceEnv logger tbl) where
    rnf !_ = ()

instance NFData WebBlockHeaderDb where
    rnf !_ = ()

instance NFData TestBlockDb where
    rnf !_ = ()

instance NFData (MempoolBackend a) where
    rnf !_ = ()

instance NFData PactQueue where
    rnf !_ = ()

instance NFData LegacyPactErrorType where
    rnf !_ = ()

instance NFData LegacyPactError where
    rnf (LegacyPactError a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d
