{-# language OverloadedStrings #-}

-- |
-- Module: Chainweb.Test.Tools.TxGen
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emmanuel Denloye-Ito <emmanuel@kadena.io>
-- Stability: experimental
--
-- Unit test for Transaction Generator via the Http Pact interface (/send,
-- etc.) (inprocess) API in Chainweb
--
module Chainweb.Test.Tools.TXGen (tests) where

import Control.Monad (replicateM, void)
import Control.Monad.IO.Class

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Fake

import Numeric.Natural


import Servant.Client

import Test.Tasty
import Test.Tasty.HUnit

-- pact imports
import Pact.Types.API
import Pact.Types.Capability
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Exp
import Pact.Types.Info (mkInfo)
import Pact.Types.Names
import Pact.Types.PactValue

-- chainweb imports
import Chainweb.Graph
import Chainweb.Test.Pact.RemotePactTest hiding (tests)
import Chainweb.Test.Pact.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version

import Data.CAS.RocksDB

import TXG.Simulate.Contracts.CoinContract
import TXG.Simulate.Contracts.Common

nNodes :: Natural
nNodes = 1

-- cid :: HasCallStack => ChainId
-- cid = head . toList $ chainIds v

v :: ChainwebVersion
v = FastTimedCPM petersonChainGraph

tests :: RocksDb -> TestTree
tests rdb =
     withNodes rdb nNodes $ \net ->
      withTime $ \iot -> txgenTest 1 iot net

txgenTest :: Int -> IO (Time Integer) -> IO ChainwebNetwork -> TestTree
txgenTest batchsize iot nio = testCaseSteps "txgen test steps" $ \step -> do

      cenv <- fmap _getClientEnv nio
      chain <- mkChainId v (0 :: Int)
      t <- iot
      accounts <- makeAccounts (toTxCreationTime t) chain
      batchTransfers <- (SubmitBatch . NEL.fromList) <$> replicateM batchsize (genTx chain accounts)

      r <- flip runClientM cenv $ do

          void $ liftIO $ step "sendApiClient: submit batch (send some transfers)"
          rksTransfers <- liftIO $ sending chain cenv batchTransfers

          void $ liftIO $ step "pollApiClient: poll until key is found (send some transfers)"
          void $ liftIO $ polling chain cenv rksTransfers ExpectPactResult
      either (assertFailure . (msg <>) . sshow) (const $ return ()) r
  where
    msg = "something failed (a better message belongs here).: "

    genTx chain accs = do
        req <- mkRandomCoinContractRequest True accs >>= generate
        let acclookup sn@(Account accsn) =
              case M.lookup sn accs of
                Just ks -> (sn, ks)
                Nothing -> error $ "Couldn't find account: <" ++ accsn ++ ">"
        let (Account sender, ks) =
              case req of
                CoinCreateAccount account (Guard guardd) -> (account, guardd)
                CoinAccountBalance account -> acclookup account
                CoinTransfer (SenderName sn) rcvr amt -> mkTransferCaps rcvr amt $ acclookup sn
                CoinTransferAndCreate (SenderName acc) rcvr (Guard guardd) amt -> mkTransferCaps rcvr amt (acc, guardd)
        meta' <- makeMetaWithSender sender chain
        let meta = meta' { _pmGasLimit = 10000 }
        createCoinContractRequest v meta ks req

    makeAccounts t chain = do
        let f (Account sender) = do
              meta' <- liftIO $ makeMetaWithSender sender chain
              let meta = meta' { _pmCreationTime = t }
              createCoinAccount v meta sender
        (coinKS, _coinAcc) <- unzip <$> traverse f coinAccountNames
        return $ buildGenAccountsKeysets (NEL.fromList coinAccountNames) (NEL.fromList coinKS)


    mkTransferCaps :: ReceiverName -> Amount -> (Account, NonEmpty SomeKeyPairCaps) -> (Account, NonEmpty SomeKeyPairCaps)
    mkTransferCaps (ReceiverName (Account r)) (Amount m) (s@(Account ss), ks) = (s, (caps <$) <$> ks)
      where caps = [gas, tfr]
            gas = SigCapability (QualifiedName "coin" "GAS" (mkInfo "coin.GAS")) []
            tfr = SigCapability (QualifiedName "coin" "TRANSFER" (mkInfo "coin.TRANSFER"))
                    [ PLiteral $ LString $ T.pack ss
                    , PLiteral $ LString $ T.pack r
                    , PLiteral $ LDecimal m]

    buildGenAccountsKeysets
      :: NonEmpty Account
      -> NonEmpty (NonEmpty SomeKeyPairCaps)
      -> Map Account (NonEmpty SomeKeyPairCaps)
    buildGenAccountsKeysets accs cks = M.fromList $ NEL.toList $ NEL.zip accs cks

-- responseGolden :: IO ChainwebNetwork -> IO RequestKeys -> TestTree
-- responseGolden networkIO rksIO = golden "remote-golden" $ do
--     rks <- rksIO
--     cenv <- _getClientEnv <$> networkIO
--     PollResponses theMap <- polling cid cenv rks ExpectPactResult
--     let values = mapMaybe (\rk -> _crResult <$> HashMap.lookup rk theMap)
--                           (NEL.toList $ _rkRequestKeys rks)
--     return $! toS $! foldMap A.encode values
