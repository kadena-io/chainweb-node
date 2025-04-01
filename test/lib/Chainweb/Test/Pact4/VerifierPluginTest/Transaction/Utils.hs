{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Test.Pact4.VerifierPluginTest.Transaction.Utils where

import Control.Concurrent.MVar
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HM
import Data.IORef
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Tasty.HUnit

-- internal modules

import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.PactError
import Pact.Types.PactValue
import Pact.Types.Pretty
import Pact.Types.RPC
import Pact.Types.Term

import Chainweb.BlockCreationTime
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.Mempool.Mempool
import Chainweb.Miner.Pact

import Chainweb.Payload
import Chainweb.Test.Cut
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Test.Pact4.Utils
import Chainweb.Test.Utils
import Chainweb.Test.TestVersions
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import Chainweb.WebPactExecutionService
import Chainweb.Payload.PayloadStore (lookupPayloadWithHeight)
import Chainweb.Pact.Types (MemPoolAccess, mpaGetBlock)

testVersion :: ChainwebVersion
testVersion = slowForkingCpmTestVersion petersen

cid :: ChainId
cid = unsafeChainId 9
    -- several tests in this file expect chain 9

data SingleEnv = SingleEnv
    { _menvBdb :: !TestBlockDb
    , _menvPact :: !WebPactExecutionService
    , _menvMpa :: !(IORef MemPoolAccess)
    , _menvMiner :: !Miner
    , _menvChainId :: !ChainId
    }

makeLenses ''SingleEnv

type PactTestM = ReaderT SingleEnv IO

newtype MempoolCmdBuilder = MempoolCmdBuilder
    { _mempoolCmdBuilder :: ChainId -> BlockCreationTime -> CmdBuilder
    }

-- | Block filler. A 'Nothing' result means "skip this filler".
newtype MempoolBlock = MempoolBlock
    { _mempoolBlock :: ChainId -> BlockCreationTime -> Maybe [MempoolCmdBuilder]
    }

-- | Mempool with an ordered list of fillers.
newtype PactMempool = PactMempool
  { _pactMempool :: [MempoolBlock]
  }
  deriving (Semigroup,Monoid)


-- | Pair a builder with a test
data PactTxTest = PactTxTest
    { _pttBuilder :: MempoolCmdBuilder
    , _pttTest :: CommandResult Hash -> Assertion
    }

checkVerifierNotInTx :: T.Text -> PactTxTest
checkVerifierNotInTx v = PactTxTest
  (buildBasic (mkExec' "(free.m.x)"))
  (\cr -> liftIO $ do
    assertTxFailure
      "verifier not present"
      ("Verifier failure " <> pretty v <> ": not in transaction")
      cr
    assertTxGas "verifier errors charge all gas" 10000 cr)

-- | Sets mempool with block fillers. A matched filler
-- (returning a 'Just' result) is executed and removed from the list.
-- Fillers are tested in order.
setPactMempool :: PactMempool -> PactTestM ()
setPactMempool (PactMempool fs) = do
  mpa <- view menvMpa
  mpsRef <- liftIO $ newIORef fs
  liftIO $ writeIORef mpa $ mempty {
    mpaGetBlock = \_ -> go mpsRef
    }
  where
    go ref mempoolPreBlockCheck bHeight bHash bct = do
      mps <- readIORef ref
      let runMps i = \case
            [] -> return mempty
            (mp:r) -> case _mempoolBlock mp cid bct of
              Just bs -> do
                writeIORef ref (take i mps ++ r)
                cmds <- fmap V.fromList $ forM bs $ \b ->
                  buildCwCmd (sshow bct) testVersion $ _mempoolCmdBuilder b cid bct
                tos <- mempoolPreBlockCheck bHeight bHash ((fmap . fmap . fmap) _pcCode cmds)
                return $ V.fromList
                  [ t
                  | Right t <- V.toList tos
                  ]
                -- return $ fmap fst $ V.filter snd (V.zip cmds validationResults)
              Nothing -> runMps (succ i) r
      runMps 0 mps

filterBlock :: (ChainId -> BlockCreationTime -> Bool) -> MempoolBlock -> MempoolBlock
filterBlock f (MempoolBlock b) = MempoolBlock $ \chain bct ->
  if f chain bct then b chain bct else Nothing

blockForChain :: ChainId -> MempoolBlock -> MempoolBlock
blockForChain chid = filterBlock $ \chain _ ->
  chain == chid

runCut' :: PactTestM ()
runCut' = do
  pact <- view menvPact
  bdb <- view menvBdb
  miner <- view menvMiner
  liftIO $ runCut testVersion bdb pact (offsetBlockTime second) zeroNoncer miner

assertTxGas :: (HasCallStack, MonadIO m) => String -> Gas -> CommandResult Hash -> m ()
assertTxGas msg g = liftIO . assertEqual msg g . _crGas

assertTxSuccess
  :: HasCallStack
  => MonadIO m
  => String
  -> PactValue
  -> CommandResult Hash
  -> m ()
assertTxSuccess msg r tx = do
  liftIO $ assertEqual msg (Just r)
    (tx ^? crResult . to _pactResult . _Right)

-- | Exact match on error doc
assertTxFailure :: (HasCallStack, MonadIO m) => String -> Doc -> CommandResult Hash -> m ()
assertTxFailure msg d tx =
  liftIO $ assertEqual msg (Just d)
    (tx ^? crResult . to _pactResult . _Left . to peDoc)

-- | Run a single mempool block on current chain with tests for each tx.
-- Limitations: can only run a single-chain, single-refill test for
-- a given cut height.
runBlockTest :: HasCallStack => [PactTxTest] -> PactTestM ()
runBlockTest pts = do
  chid <- view menvChainId
  setPactMempool $ PactMempool [testsToBlock chid pts]
  runCut'
  runBlockTests pts

-- | Convert tests to block for specified chain.
testsToBlock :: ChainId -> [PactTxTest] -> MempoolBlock
testsToBlock chid pts = blockForChain chid $ MempoolBlock $ \_ _ ->
  pure $ map _pttBuilder pts

-- | Run tests on current cut and chain.
runBlockTests :: HasCallStack => [PactTxTest] -> PactTestM ()
runBlockTests pts = do
  rs <- txResults
  liftIO $ assertEqual "Result length should equal transaction length" (length pts) (length rs)
  zipWithM_ go pts (V.toList rs)
  where
    go :: PactTxTest -> CommandResult Hash -> PactTestM ()
    go (PactTxTest _ t) cr = liftIO $ t cr

-- | Run cuts to block height.
runToHeight :: BlockHeight -> PactTestM ()
runToHeight bhi = do
  chid <- view menvChainId
  bh <- getHeader chid
  when (view blockHeight bh < bhi) $ do
    runCut'
    runToHeight bhi

signSender00 :: CmdBuilder -> CmdBuilder
signSender00 = set cbSigners [mkEd25519Signer' sender00 []]

setFromHeader :: BlockHeader -> CmdBuilder -> CmdBuilder
setFromHeader bh =
  set cbChainId (_chainId bh)
  . set cbCreationTime (toTxCreationTime $ _bct $ view blockCreationTime bh)

buildBasic
    :: PactRPC T.Text
    -> MempoolCmdBuilder
buildBasic = buildBasic' id

buildBasicGas :: GasLimit -> PactRPC T.Text -> MempoolCmdBuilder
buildBasicGas g = buildBasic' (set cbGasLimit g)

-- | Build with specified setter to mutate defaults.
buildBasic'
    :: (CmdBuilder -> CmdBuilder)
    -> PactRPC T.Text
    -> MempoolCmdBuilder
buildBasic' f r = MempoolCmdBuilder $ \chain bct ->
  f $ signSender00
  $ set cbChainId chain
  $ set cbCreationTime (toTxCreationTime $ _bct bct)
  $ set cbRPC r
  $ defaultCmd

-- | Get output on latest cut for chain
getPWO :: ChainId -> PactTestM (PayloadWithOutputs,BlockHeader)
getPWO chid = do
  (TestBlockDb _ pdb _) <- view menvBdb
  h <- getHeader chid
  Just pwo <- liftIO $ lookupPayloadWithHeight pdb (Just $ view blockHeight h) (view blockPayloadHash h)
  return (pwo,h)

getHeader :: ChainId -> PactTestM BlockHeader
getHeader chid = do
  (TestBlockDb _ _ cmv) <- view menvBdb
  c <- liftIO $ readMVar cmv
  fromMaybeM (userError $ "chain lookup failed for " ++ show chid) $ HM.lookup chid (_cutMap c)

txResults :: HasCallStack => PactTestM (V.Vector (CommandResult Hash))
txResults = do
  chid <- view menvChainId
  (o,_h) <- getPWO chid
  forM (_payloadWithOutputsTransactions o) $ \(_,txo) ->
    decodeStrictOrThrow @_ @(CommandResult Hash) (_transactionOutputBytes txo)
