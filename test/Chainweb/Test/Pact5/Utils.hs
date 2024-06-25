module Chainweb.Test.Pact5.Utils
    ( initCheckpointer
    )
    where

import Chainweb.Logger
import Chainweb.Pact.Backend.RelationalCheckpointer
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Types
import Chainweb.Version
import qualified Data.Text as T
import System.LogLevel

initCheckpointer :: ChainwebVersion -> ChainId -> SQLiteEnv -> IO (Checkpointer GenericLogger)
initCheckpointer v cid sql = do
    initRelationalCheckpointer defaultModuleCacheLimit sql DoNotPersistIntraBlockWrites (genericLogger Error (error . T.unpack)) v cid
