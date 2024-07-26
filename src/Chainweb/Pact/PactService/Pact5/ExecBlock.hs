{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Chainweb.Pact.PactService.Pact5.ExecBlock
    (

    ) where

import Chainweb.Logger
import Chainweb.Miner.Pact
import Chainweb.Pact.Service.Types
import Chainweb.Pact.Types hiding (ctxCurrentBlockHeight, TxContext(..))
import qualified Chainweb.Pact5.Transaction as Pact5
import Chainweb.Payload
import Data.ByteString (ByteString)
import Data.Decimal
import Data.Vector (Vector)
import Data.Void
import qualified Pact.Core.Command.Types as Pact5
import qualified Pact.Core.Persistence as Pact5
import qualified Pact.Parse as Pact4
import Chainweb.Pact5.NoCoinbase
import Control.Lens
import Chainweb.Version
import Data.Default
import Control.Monad.IO.Class
import Chainweb.BlockHeight
import qualified Data.Map as Map
import Chainweb.Utils (int, Codec (..))
import Numeric.Natural
import qualified Chainweb.Pact5.TransactionExec as Pact5
import Chainweb.Pact5.Types
import qualified Data.Vector as V
import Data.Foldable
import Data.Either (partitionEithers)
import Control.Monad
import qualified Data.Text as T
import Control.Exception.Safe
import Control.Exception (evaluate)
import Control.DeepSeq

-- | Calculate miner reward. We want this to error hard in the case where
-- block times have finally exceeded the 120-year range. Rewards are calculated
-- at regular blockheight intervals.
--
-- See: 'rewards/miner_rewards.csv'
--
minerReward
    :: ChainwebVersion
    -> MinerRewards
    -> BlockHeight
    -> IO Decimal
minerReward v (MinerRewards rs) bh =
    case Map.lookupGE bh rs of
      Nothing -> err
      Just (_, m) -> pure $! roundTo 8 (m / n)
  where
    !n = int @Natural @Decimal . order $ chainGraphAt v bh
    err = internalError "block heights have been exhausted"
{-# INLINE minerReward #-}


runPact5Coinbase
    :: (Logger logger)
    => Bool
    -> Miner
    -> PactBlockM logger Pact5Db tbl (Either Pact5CoinbaseError (Pact5.CommandResult [Pact5.TxLog ByteString] Void))
runPact5Coinbase True _ = return (Right noCoinbase)
runPact5Coinbase False miner = do
    logger <- view (psServiceEnv . psLogger)
    rs <- view (psServiceEnv . psMinerRewards)
    v <- view chainwebVersion
    txCtx <- TxContext <$> view psParentHeader <*> pure miner

    let !bh = ctxCurrentBlockHeight txCtx

    reward <- liftIO $! minerReward v rs bh
    pactDb <- view (psBlockDbEnv . cpPactDbEnv)

    liftIO $ Pact5.applyCoinbase logger pactDb reward txCtx
  where
    upgradeInitCache newCache = do
      liftPactServiceM $ logInfo "Updating init cache for upgrade"
      updateInitCacheM newCache

pact5TransactionsFromPayload
    :: PayloadData
    -> IO (Vector Pact5.Transaction)
pact5TransactionsFromPayload plData = do
    vtrans <- fmap V.fromList $
              mapM toCWTransaction $
              toList (_payloadDataTransactions plData)
    let (theLefts, theRights) = partitionEithers $ V.toList vtrans
    unless (null theLefts) $ do
        let ls = map T.pack theLefts
        throwM $ TransactionDecodeFailure $ "Failed to decode pact transactions: "
            <> T.intercalate ". " ls
    return $! V.fromList theRights
  where
    toCWTransaction bs = evaluate (force (codecDecode Pact5.payloadCodec $
                                          _transactionBytes bs))
