{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>, Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- Pact Service SPV Support roundtrip tests
--
module Chainweb.Test.Pact4.SPV
( -- * test suite
  tests
  -- * repl tests
, standard
, invalidProof
) where

import Control.Arrow ((***))
import Control.Concurrent.MVar
import Control.Exception (SomeException, finally)
import Control.Monad
import Control.Lens hiding ((.=))

import Data.Aeson as Aeson
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as B8

import Data.ByteString.Lazy (toStrict)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.List (isInfixOf)
import Data.LogMessage
import Data.Text (pack,Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word

import Ethereum.Block
import Ethereum.Receipt
import Ethereum.Receipt.ReceiptProof
import Ethereum.RLP

import System.LogLevel

import Test.Tasty
import Test.Tasty.HUnit

-- internal pact modules

import qualified Pact.JSON.Encode as J
import Pact.Types.Command
import Pact.Types.Exp
import Pact.Types.Hash
import Pact.Types.PactValue
import Pact.Types.Runtime (toPactId)
import Pact.Types.SPV
import Pact.Types.Term

-- internal chainweb modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Cut
import Chainweb.Graph
import Chainweb.Miner.Pact

import Chainweb.Pact.Types (MemPoolAccess, mpaGetBlock)
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.SPV.CreateProof
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.Cut
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Test.Pact4.Utils
import Chainweb.Test.Utils
import Chainweb.Test.TestVersions
import Chainweb.Time
import qualified Chainweb.Pact4.Transaction as Pact4
import Chainweb.Utils hiding (check)
import Chainweb.Version as Chainweb
import Chainweb.WebPactExecutionService
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class


-- | Note: These tests are intermittently non-deterministic due to the way
-- random chain sampling works with our test harnesses.
--
tests :: RocksDb -> TestTree
tests rdb = testGroup "Chainweb.Test.Pact4.SPV"
    [ testCaseSteps "standard SPV verification round trip" $ standard rdb
    , testCaseSteps "contTXOUTOld" $ contTXOUTOld rdb
    , testCaseSteps "contTXOUTNew" $ contTXOUTNew rdb
    , testCaseSteps "tfrTXOUTNew" $ tfrTXOUTNew rdb
    , testCaseSteps "ethReceiptProof" $ ethReceiptProof rdb
    , testCaseSteps "noEthReceiptProof" $ noEthReceiptProof rdb
    , testCaseSteps "invalid proof formats fail" $ invalidProof rdb
    ]

testVer :: ChainwebVersion
testVer = noBridgeCpmTestVersion triangleChainGraph

bridgeVer :: ChainwebVersion
bridgeVer = fastForkingCpmTestVersion pairChainGraph

-- Only use for debugging. Do not use in tests in the test suite!
--
logg :: LogMessage a => LogLevel -> a -> IO ()
logg l
  | l <= Warn = T.putStrLn . logText
  | otherwise = const $ return ()


-- debugging
_handle' :: SomeException -> IO (Bool, String)
_handle' e =
    let
      s = show e
    in logg System.LogLevel.Error (pack s) >> return (False, s)

-- -------------------------------------------------------------------------- --
-- tests

standard :: RocksDb -> (String -> IO ()) -> Assertion
standard rdb step = do
  (c1,c3) <- roundtrip rdb 0 1 burnGen createSuccess step
  checkResult c1 0 "ObjectMap"
  checkResult c3 1 "Write succeeded"

contTXOUTOld :: RocksDb -> (String -> IO ()) -> Assertion
contTXOUTOld rdb step = do
  code <- T.readFile "test/pact/contTXOUTOld.pact"
  (c1,c3) <- roundtrip rdb 0 1 burnGen (createVerify False code mdata) step
  checkResult c1 0 "ObjectMap"
  checkResult' c3 1 $ PactResult $ Right $ PLiteral $ LString rSuccessTXOUT
  where
    mdata = toJSON [fst sender01] :: Value


contTXOUTNew :: RocksDb -> (String -> IO ()) -> Assertion
contTXOUTNew rdb step = do
  code <- T.readFile "test/pact/contTXOUTNew.pact"
  (c1,c3) <- roundtrip' rdb bridgeVer 0 1 burnGen (createVerify True code mdata) step
  checkResult c1 0 "ObjectMap"
  checkResult' c3 1 $ PactResult $ Right $ PLiteral $ LString rSuccessTXOUT
  where
    mdata = toJSON [fst sender01]


tfrTXOUTNew :: RocksDb -> (String -> IO ()) -> Assertion
tfrTXOUTNew rdb step = do
  code <- T.readFile "test/pact/tfrTXOUTNew.pact"
  (c1,c3) <- roundtrip' rdb bridgeVer 0 1 transferGen (createVerify True code mdata) step
  checkResult c1 0 "Write succeeded"
  checkResult' c3 1 $ PactResult $ Right $ PLiteral $ LString rSuccessTXOUT
  where
    mdata = toJSON [fst sender01] :: Value

ethReceiptProof :: RocksDb -> (String -> IO ()) -> Assertion
ethReceiptProof rdb step = do
  code <- T.readFile "test/pact/ethReceiptProof.pact"
  (c1,c3) <- roundtrip' rdb bridgeVer 0 1 transferGen (createVerifyEth code) step
  checkResult c1 0 "Write succeeded"
  checkResult' c3 1 $ PactResult $ Right $ PLiteral $ LString "ETH Success"

noEthReceiptProof :: RocksDb -> (String -> IO ()) -> Assertion
noEthReceiptProof rdb step = do
  code <- T.readFile "test/pact/ethReceiptProof.pact"
  (c1,c3) <- roundtrip' rdb testVer 0 1 transferGen (createVerifyEth code) step
  checkResult c1 0 "Write succeeded"
  checkResult c3 1 "unsupported SPV types: ETH"

rSuccessTXOUT :: Text
rSuccessTXOUT = "TXOUT Success"

invalidProof :: RocksDb -> (String -> IO ()) -> Assertion
invalidProof rdb step = do
  (c1,c3) <- roundtrip rdb 0 1 burnGen createInvalidProof step
  checkResult c1 0 "ObjectMap"
  checkResult c3 1 "Failure: resumePact: no previous execution found"

checkResult :: HasCallStack => CutOutputs -> Word32 -> String -> Assertion
checkResult co ci expect =
  assertSatisfies ("result on chain " ++ show ci ++ " contains '" ++ show expect ++ "'")
    (HM.lookup (unsafeChainId ci) co) (isInfixOf expect . show)

checkResult' :: HasCallStack => CutOutputs -> Word32 -> PactResult -> Assertion
checkResult' co ci expect = case HM.lookup (unsafeChainId ci) co of
  Nothing -> assertFailure $ "No result found for chain " ++ show ci
  Just v -> case Vector.toList v of
    [(_,cr)] -> assertEqual "pact results match" expect (_crResult cr)
    _ -> assertFailure $ "expected single result, got " ++ show v

getCutOutputs :: TestBlockDb -> IO CutOutputs
getCutOutputs (TestBlockDb _ pdb cmv) = do
  c <- readMVar cmv
  cutToPayloadOutputs c pdb

-- | Populate blocks for every chain of the current cut. Uses provided pact
-- service to produce a new block, add it
runCut' :: ChainwebVersion -> TestBlockDb -> WebPactExecutionService -> IO CutOutputs
runCut' v bdb pact = do
  runCut v bdb pact (offsetBlockTime second) zeroNoncer noMiner
  getCutOutputs bdb

roundtrip
    :: RocksDb
    -> Word32
      -- ^ source chain id
    -> Word32
      -- ^ target chain id
    -> BurnGenerator
      -- ^ burn tx generator
    -> CreatesGenerator
      -- ^ create tx generator
    -> (String -> IO ())
    -> IO (CutOutputs, CutOutputs)
roundtrip rdb = roundtrip' rdb testVer

roundtrip'
    :: RocksDb
    -> ChainwebVersion
    -> Word32
      -- ^ source chain id
    -> Word32
      -- ^ target chain id
    -> BurnGenerator
      -- ^ burn tx generator
    -> CreatesGenerator
      -- ^ create tx generator
    -> (String -> IO ())
      -- ^ logging backend
    -> IO (CutOutputs, CutOutputs)
roundtrip' rdb v sid0 tid0 burn create step = runResourceT $ do
  bdb <- mkTestBlockDb v rdb
  liftIO $ do
    tg <- newMVar mempty
    let logger = hunitDummyLogger step
    mempools <- onAllChains v $ \chain ->
      return $ chainToMPA' chain tg
    withWebPactExecutionService logger v testPactServiceConfig bdb mempools $ \(pact,_) -> do

      sid <- mkChainId v maxBound sid0
      tid <- mkChainId v maxBound tid0

      -- track the continuation pact id
      pidv <- newEmptyMVar @PactId

      -- cut 0: empty run (not sure why this is needed but test fails without it)
      step "cut 0: empty run"
      void $ runCut' v bdb pact

      -- cut 1: burn
      step "cut 1: burn"
      -- Creating the parent took at least 1 second. So 1s is fine as creation time
      let t1 = add second epoch
      txGen1 <- burn v t1 pidv sid tid
      void $ swapMVar tg txGen1
      co1 <- runCut' v bdb pact

      -- setup create txgen with cut 1
      step "setup create txgen with cut 1"
      (BlockCreationTime t2) <- view blockCreationTime <$> getParentTestBlockDb bdb tid
      hi <- view blockHeight <$> getParentTestBlockDb bdb sid
      txGen2 <- create v t2 bdb pidv sid tid hi

      -- cut 2: empty cut for diameter 1
      step "cut 2: empty cut for diameter 1"
      void $ swapMVar tg mempty
      void $ runCut' v bdb pact

      -- cut 3: create
      step "cut 3: create"
      void $ swapMVar tg txGen2
      co2 <- runCut' v bdb pact

      return (co1,co2)


_debugCut :: CanReadablePayloadCas tbl => String -> Cut -> PayloadDb tbl -> IO ()
_debugCut msg c pdb = do
  putStrLn $ "CUT: =============== " ++ msg
  outs <- cutToPayloadOutputs c pdb
  forM_ (HM.toList outs) $ \(cid,vs) -> do
    putStrLn $ "Chain: " ++ show cid
    forM_ vs $ \(cmd,pr) -> do
      putStrLn $ show (_cmdHash cmd) ++ ": " ++ show pr

type CutOutputs = HM.HashMap Chainweb.ChainId (Vector (Command Text, CommandResult Hash))

cutToPayloadOutputs
  :: CanReadablePayloadCas tbl
  => Cut
  -> PayloadDb tbl
  -> IO CutOutputs
cutToPayloadOutputs c pdb = do
  forM (_cutMap c) $ \bh -> do
    Just pwo <- lookupPayloadWithHeight pdb (Just $ view blockHeight bh) (view blockPayloadHash bh)
    let txs = Vector.map (toTx *** toCR) (_payloadWithOutputsTransactions pwo)
        toTx :: Transaction -> Command Text
        toTx (Transaction t) = fromJuste $ decodeStrict' t
        toCR :: TransactionOutput -> CommandResult Hash
        toCR (TransactionOutput t) = fromJuste $ decodeStrict' t
    return txs

chainToMPA' :: ChainId -> MVar TransactionGenerator -> MemPoolAccess
chainToMPA' chain f = mempty
    { mpaGetBlock = \_g pc hi ha he -> do
        tg <- readMVar f
        txs <- tg chain hi ha he
        tos <- pc hi ha ((fmap . fmap . fmap) _pcCode txs)
        forM tos $ \case
          Left err -> error (sshow err)
          Right t -> return t
    }


-- -------------------------------------------------------------------------- --
-- transaction generators

type TransactionGenerator
    = Chainweb.ChainId
    -> BlockHeight
    -> BlockHash
    -> BlockCreationTime
    -> IO (Vector Pact4.Transaction)

type BurnGenerator
    = ChainwebVersion -> Time Micros -> MVar PactId -> Chainweb.ChainId -> Chainweb.ChainId -> IO TransactionGenerator

type CreatesGenerator
    = ChainwebVersion
    -> Time Micros
    -> TestBlockDb
    -> MVar PactId
    -> Chainweb.ChainId
    -> Chainweb.ChainId
    -> BlockHeight
    -> IO TransactionGenerator

-- | Generate burn/create Pact Service commands on arbitrarily many chains
--
burnGen :: BurnGenerator
burnGen v time pidv sid tid = do
    ref0 <- newIORef False
    ref1 <- newIORef False
    return $ go ref0 ref1
  where
    go ref0 ref1 _cid _bhe _bha _
      | sid /= _cid = return mempty
      | otherwise = readIORef ref0 >>= \case
        True -> return mempty
        False -> do
            readIORef ref1 >>= \case
              True -> return mempty
              False -> do
                cmd <- buildCwCmd "0" v $
                  set cbSigners [mkEd25519Signer' sender00 []] $
                  set cbCreationTime (toTxCreationTime time) $
                  set cbChainId sid $
                  set cbRPC (mkExec tx1Code tx1Data) $
                  defaultCmd
                writeIORef ref0 True

                let pid = toPactId $ toUntypedHash $ _cmdHash cmd

                putMVar pidv pid `finally` writeIORef ref1 True
                return $ Vector.singleton cmd

    tx1Code = T.unlines
      [ "(coin.transfer-crosschain"
      , "  'sender00"
      , "  'sender01"
      , "  (read-keyset 'sender01-keyset)"
      , "  (read-msg 'target-chain-id)"
      , "  1.0)"
      ]

    tx1Data =
      -- sender01 keyset guard
      let ks = mkKeySet
            ["6be2f485a7af75fedb4b7f153a903f7e6000ca4aa501179c91a2450b777bd2a7"]
            "keys-all"

      in object
         [ "sender01-keyset" .= J.toJsonViaEncode ks
         , "target-chain-id" .= chainIdToText tid
         ]

-- | Generate arbitrary coin.transfer call.
--
transferGen :: BurnGenerator
transferGen v time pidv sid _tid = do
    ref0 <- newIORef False
    ref1 <- newIORef False
    return $ go ref0 ref1
  where
    go ref0 ref1 _cid _bhe _bha _
      | sid /= _cid = return mempty
      | otherwise = readIORef ref0 >>= \case
        True -> return mempty
        False -> do
            readIORef ref1 >>= \case
              True -> return mempty
              False -> do
                cmd <- buildCwCmd "0" v $
                  set cbSigners
                    [mkEd25519Signer' sender00
                       [mkTransferCap "sender00" "sender01" 1.0
                       ,mkGasCap]] $
                  set cbCreationTime (toTxCreationTime time) $
                  set cbChainId sid $
                  set cbRPC (mkExec' tx1Code) $
                  defaultCmd
                writeIORef ref0 True

                let pid = toPactId $ toUntypedHash $ _cmdHash cmd

                putMVar pidv pid `finally` writeIORef ref1 True
                return $ Vector.singleton cmd



    tx1Code = "(coin.transfer 'sender00 'sender01 1.0)"

createCont
  :: ChainwebVersion
  -> ChainId
  -> MVar PactId
  -> Maybe ContProof
  -> Time Micros
  -> IO (Vector Pact4.Transaction)
createCont v cid pidv proof time = do
  pid <- readMVar pidv
  fmap Vector.singleton $
    buildCwCmd "1" v $
    set cbSigners [mkEd25519Signer' sender00 []] $
    set cbCreationTime (toTxCreationTime time) $
    set cbChainId cid $
    set cbRPC (mkCont $ (mkContMsg pid 1) { _cmProof = proof }) $
    defaultCmd

-- | Generate a tx to run 'verify-spv' tests.
--
createVerify :: Bool -> Text -> Value -> CreatesGenerator
createVerify bridge code mdata v time (TestBlockDb wdb pdb _c) _pidv sid tid bhe = do
    ref <- newIORef False
    return $ go ref
  where
    go ref cid _bhe _bha _
        | tid /= cid = return mempty
        | otherwise = readIORef ref >>= \case
            True -> return mempty
            False -> do
                pf <- createTransactionOutputProof_ wdb pdb tid sid bhe 0
                let q | bridge = object
                        [ ("proof", String $ encodeB64UrlNoPaddingText $ encodeToByteString pf)
                        ]
                      | otherwise = toJSON pf
                cmd <- buildCwCmd "0" v $
                  set cbSigners [mkEd25519Signer' sender00 []] $
                  set cbCreationTime (toTxCreationTime time) $
                  set cbChainId tid $
                  set cbRPC (mkExec code (object [("proof",q),("data",mdata)])) $
                  defaultCmd
                return (Vector.singleton cmd)
                    `finally` writeIORef ref True

-- | Generate a tx to run 'verify-spv' tests.
--
createVerifyEth :: Text -> CreatesGenerator
createVerifyEth code v time (TestBlockDb _wdb _pdb _c) _pidv _sid tid _bhe = do
    ref <- newIORef False
    q <- encodeB64UrlNoPaddingText . putRlpByteString <$> receiptProofTest 2
    return $ go q ref
  where
    go q ref cid _bhe _bha _
        | tid /= cid = return mempty
        | otherwise = readIORef ref >>= \case
            True -> return mempty
            False -> do
                -- q <- toJSON <$> createTransactionOutputProof_ wdb pdb tid sid bhe 0
                cmd <- buildCwCmd "0" v $
                  set cbSigners [mkEd25519Signer' sender00 []] $
                  set cbCreationTime (toTxCreationTime time) $
                  set cbChainId tid $
                  set cbRPC (mkExec code (object [("proof", toJSON q)])) $
                  defaultCmd
                return (Vector.singleton cmd)
                    `finally` writeIORef ref True

receiptProofTest :: Int -> IO ReceiptProof
receiptProofTest i = do
    recps <- readFile "test/pact/receipts.json"
    blk <- readFile "test/pact/block.json"
    rs <- decodeStrictOrThrow @_ @[RpcReceipt] $ B8.pack recps
    block <- decodeStrictOrThrow @_ @RpcBlock $ B8.pack blk
    let hdr = _rpcBlockHeader block
    rpcReceiptProof hdr [] rs (TransactionIndex $ fromIntegral i)


-- | Generate the 'create-coin' command in response to the previous 'delete-coin' call.
-- Note that we maintain an atomic update to make sure that if a given chain id
-- has already called the 'create-coin' half of the transaction, it will not do so again.
--
createSuccess :: CreatesGenerator
createSuccess v time (TestBlockDb wdb pdb _c) pidv sid tid bhe = do
    ref <- newIORef False
    return $ go ref
  where
    go ref cid _bhe _bha _
        | tid /= cid = return mempty
        | otherwise = readIORef ref >>= \case
            True -> return mempty
            False -> do
                q <- toJSON <$> createTransactionOutputProof_ wdb pdb tid sid bhe 0
                let proof = Just . ContProof .  B64U.encode . toStrict . Aeson.encode $ q
                createCont v tid pidv proof time
                    `finally` writeIORef ref True

-- | Execute create-coin command with invalid proof
--
createInvalidProof :: CreatesGenerator
createInvalidProof v time _ pidv _ tid _ = do
    ref <- newIORef False
    return $ go ref
  where
    go ref cid _bhe _bha _
        | tid /= cid = return mempty
        | otherwise = readIORef ref >>= \case
            True -> return mempty
            False ->
                createCont v tid pidv Nothing time
                    `finally` writeIORef ref True
