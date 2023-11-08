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
-- Module: Chainweb.Test.CutDB.Test
-- Copyright: Copyright © 2018 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>, Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- Pact Service SPV Support roundtrip tests
--
module Chainweb.Test.Pact.SPV
( -- * test suite
  tests
  -- * repl tests
, standard
, wrongChain
, wrongChainProof
, invalidProof
) where

import Control.Arrow ((***))
import Control.Concurrent.MVar
import Control.Exception (SomeException, finally)
import Control.Monad
import Control.Lens hiding ((.=))
import Control.Lens (set)
import Control.Monad.Trans.Except

import Data.Aeson as Aeson
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as B8

import Data.Default (def)
import Data.ByteString.Lazy (toStrict)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.List (isInfixOf)
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
import Pact.Types.Runtime (toPactId, Type(TyAny))
import Pact.Types.SPV
import Pact.Types.Term
import Pact.Types.Util (AsString(..))

-- internal chainweb modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.Cut
import Chainweb.Graph
import Chainweb.Miner.Pact
import Chainweb.Pact.SPV
import Chainweb.Pact.Backend.Types
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.SPV.CreateProof
import Chainweb.Test.Cut
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Test.TestVersions
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Utils hiding (check)
import Chainweb.Version as Chainweb
import Chainweb.WebPactExecutionService

import Chainweb.Storage.Table (casLookupM)

import Data.LogMessage

-- | Note: These tests are intermittently non-deterministic due to the way
-- random chain sampling works with our test harnesses.
--
tests :: TestTree
tests = testGroup "Chainweb.Test.Pact.SPV"
    -- [ testCaseSteps "standard SPV verification round trip" standard
    -- , testCaseSteps "contTXOUTOld" contTXOUTOld
    -- , testCaseSteps "contTXOUTNew" contTXOUTNew
    -- , testCaseSteps "tfrTXOUTNew" tfrTXOUTNew
    -- , testCaseSteps "ethReceiptProof" ethReceiptProof
    -- , testCaseSteps "noEthReceiptProof" noEthReceiptProof
    -- , testCaseSteps "wrong chain execution fails" wrongChain
    -- , testCaseSteps "invalid proof formats fail" invalidProof
    -- , testCaseSteps "wrong target chain in proofs fail" wrongChainProof
    [ testGroup "hyperlane"
      [ testCase "empty object" hyperlaneEmptyObject

      , testCase "encodeTokenMessageERC20" hyperlaneEncodeTokenMessageERC20
      , testCase "decodeTokenMessageERC20" hyperlaneDecodeTokenMessageERC20

      , testCase "encodeHyperlaneMessage" hyperlaneEncodeHyperlaneMessage
      , testCase "decodeHyperlaneMessage" hyperlaneDecodeHyperlaneMessage

      , testCase "verifyHyperlaneMessage" hyperlaneVerifyHyperlaneMessage
      ]
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


hyperlaneEmptyObject :: Assertion
hyperlaneEmptyObject = do
  let obj = mkObject []
  res <- runExceptT $ evalHyperlaneCommand obj
  assertEqual "should fail with missing command name" (Left "Unknown hyperlane command") res

hyperlaneEncodeTokenMessageERC20 :: Assertion
hyperlaneEncodeTokenMessageERC20 = do
  let
    res = encodeTokenMessageERC20 $ mkObject
        [ ("recipient", tStr $ asString ("recipient" :: Text))
          , ("amount", tLit $ LInteger 23) ]
  case res of
    Nothing -> assertFailure "Should get the result"
    Just t ->
      let
        expectedMessage :: Text = "0x000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000170000000000000000000000000000000000000000000000000000000000000009726563697069656e740000000000000000000000000000000000000000000000"
      in assertEqual "should get encoded message" t expectedMessage

hyperlaneDecodeTokenMessageERC20 :: Assertion
hyperlaneDecodeTokenMessageERC20 = do
  let
    encodedMessage :: Text = "0x000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000170000000000000000000000000000000000000000000000000000000000000009726563697069656e740000000000000000000000000000000000000000000000"
    obj' = mkObject
        [ ("cmd", tStr $ asString ("decodeTokenMessageERC20" :: Text))
        , ("arg", obj [ ("encodedMessage", tStr $ asString encodedMessage) ])
        ]
  res <- runExceptT $ evalHyperlaneCommand obj'

  let
    expectedObject = mkObject
      [ ("tokenMessageERC20", obj
        [ ("recipient", tStr $ asString ("recipient" :: Text))
        , ("amount", tLit $ LInteger 23) ])
      ]
  case res of
    Left _ -> assertFailure "Should get the result"
    Right o -> assertEqual "Should properly decode the object" expectedObject o

hyperlaneEncodeHyperlaneMessage :: Assertion
hyperlaneEncodeHyperlaneMessage = do
  let
    obj' = mkObject
        [ ("message", obj
          [ ("version", tLit $ LInteger 1)
          , ("nonce", tLit $ LInteger 1223)
          , ("originDomain", tLit $ LInteger 626)
          , ("sender", tStr $ asString ("MTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTI=" :: Text))
          , ("destinationDomain", tLit $ LInteger 8)
          , ("recipient", tStr $ asString ("MjIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTI=" :: Text))
          , ("tokenMessage", obj
              [ ("recipient", tStr $ asString ("recipient1" :: Text))
              , ("amount", tLit $ LInteger 12) ]
            )
          ])
        ]
  res <- runExceptT $ evalHyperlaneCommand obj'
  case res of
    Left err -> assertFailure $ "Should get the result" ++ show err
    Right o ->
      let
        expectedMessage :: Text = "0x01000004c7000002723132333435363738393031323334353637383930313233343536373839303132000000083232333435363738393031323334353637383930313233343536373839303132307830303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303430303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030633030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030306137323635363336393730363936353665373433313030303030303030303030303030303030303030303030303030303030303030303030303030303030303030"
        expectedObject = mkObject
          [ ("encodedMessage", tStr $ asString expectedMessage)
          , ("messageId", tStr $ asString ("0x44913bf0252502307d973a4da0a34b12c476594481da13235989af05e8086a5a" :: Text))
          ]
      in assertEqual "should get encoded message" expectedObject o

hyperlaneDecodeHyperlaneMessage :: Assertion
hyperlaneDecodeHyperlaneMessage = do
  let
    encodedMessage :: Text = "0x01000004c7000002723132333435363738393031323334353637383930313233343536373839303132000000083232333435363738393031323334353637383930313233343536373839303132307830303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303430303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030633030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030306137323635363336393730363936353665373433313030303030303030303030303030303030303030303030303030303030303030303030303030303030303030"
    obj' = mkObject
        [ ("cmd", tStr $ asString ("decodeHyperlaneMessage" :: Text))
        , ("arg", obj [ ("encodedMessage", tStr $ asString encodedMessage) ])
        ]
  res <- runExceptT $ evalHyperlaneCommand obj'

  let
    tokenMessage :: Text = "0x0000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000000a726563697069656e743100000000000000000000000000000000000000000000"
    expectedObject = mkObject
      [ ("hyperlaneMessage", obj
        [ ("version", tLit $ LInteger 1)
          , ("nonce", tLit $ LInteger 1223)
          , ("originDomain", tLit $ LInteger 626)
          , ("sender", tStr $ asString ("0x3132333435363738393031323334353637383930313233343536373839303132" :: Text))
          , ("destinationDomain", tLit $ LInteger 8)
          , ("recipient", tStr $ asString ("0x3232333435363738393031323334353637383930313233343536373839303132" :: Text))
          , ("messageBody", tStr $ asString tokenMessage) ])
      , ("messageId", tStr $ asString ("0x44913bf0252502307d973a4da0a34b12c476594481da13235989af05e8086a5a" :: Text))
      ]
  case res of
    Left _ -> assertFailure "Should get the result"
    Right o -> assertEqual "Should properly decode the object" expectedObject o

hyperlaneVerifyHyperlaneMessage :: Assertion
hyperlaneVerifyHyperlaneMessage = do
  let
    encodedMessage :: Text = "0x01000004c7000002723132333435363738393031323334353637383930313233343536373839303132000000083232333435363738393031323334353637383930313233343536373839303132307830303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303430303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030633030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030306137323635363336393730363936353665373433313030303030303030303030303030303030303030303030303030303030303030303030303030303030303030"
    encodedMetadata :: Text = "0x6f726967696e4d65726b6c6554726565416464726573730000000000000000006d65726b6c65526f6f740000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffff000000000000000000000000000000000000000000000000000000000000008000000000000000000000000000000000000000000000000000000000000000414e45a1dc8d84b3a63db8c9c6cbe4e0a780ecb55ff157c038b9f5d1a2be7a0a0277c3c8d8e6029f65f7f7b0ad8b80fae2b178d14c9a7b228a539349aad0c7b58b1b00000000000000000000000000000000000000000000000000000000000000"
    obj' = mkObject
        [ ("cmd", tStr $ asString ("verify" :: Text))
        , ("arg", obj
          [ ("encodedMessage", tStr $ asString encodedMessage)
          , ("encodedMetadata", tStr $ asString encodedMetadata)
          ])
        ]
  res <- runExceptT $ evalHyperlaneCommand obj'

  let
    expectedObject = mkObject
      [ ("addresses", toTList TyAny def [ tStr $ asString ("0xd3884b49b90a6344ee898ac6034f443b4975d9c4" :: Text) ])
      ]
  case res of
    Left _ -> assertFailure "Should get the result"
    Right o -> assertEqual "Should properly decode the object" expectedObject o

-- -------------------------------------------------------------------------- --
-- tests

standard :: (String -> IO ()) -> Assertion
standard step = do
  (c1,c3) <- roundtrip 0 1 burnGen createSuccess step
  checkResult c1 0 "ObjectMap"
  checkResult c3 1 "Write succeeded"

contTXOUTOld :: (String -> IO ()) -> Assertion
contTXOUTOld step = do
  code <- T.readFile "test/pact/contTXOUTOld.pact"
  (c1,c3) <- roundtrip 0 1 burnGen (createVerify False code mdata) step
  checkResult c1 0 "ObjectMap"
  checkResult' c3 1 $ PactResult $ Right $ PLiteral $ LString rSuccessTXOUT
  where
    mdata = toJSON [fst sender01] :: Value


contTXOUTNew :: (String -> IO ()) -> Assertion
contTXOUTNew step = do
  code <- T.readFile "test/pact/contTXOUTNew.pact"
  (c1,c3) <- roundtrip' bridgeVer 0 1 burnGen (createVerify True code mdata) step
  checkResult c1 0 "ObjectMap"
  checkResult' c3 1 $ PactResult $ Right $ PLiteral $ LString rSuccessTXOUT
  where
    mdata = toJSON [fst sender01]


tfrTXOUTNew :: (String -> IO ()) -> Assertion
tfrTXOUTNew step = do
  code <- T.readFile "test/pact/tfrTXOUTNew.pact"
  (c1,c3) <- roundtrip' bridgeVer 0 1 transferGen (createVerify True code mdata) step
  checkResult c1 0 "Write succeeded"
  checkResult' c3 1 $ PactResult $ Right $ PLiteral $ LString rSuccessTXOUT
  where
    mdata = toJSON [fst sender01] :: Value

ethReceiptProof :: (String -> IO ()) -> Assertion
ethReceiptProof step = do
  code <- T.readFile "test/pact/ethReceiptProof.pact"
  (c1,c3) <- roundtrip' bridgeVer 0 1 transferGen (createVerifyEth code) step
  checkResult c1 0 "Write succeeded"
  checkResult' c3 1 $ PactResult $ Right $ PLiteral $ LString "ETH Success"


noEthReceiptProof :: (String -> IO ()) -> Assertion
noEthReceiptProof step = do
  code <- T.readFile "test/pact/ethReceiptProof.pact"
  (c1,c3) <- roundtrip' testVer 0 1 transferGen (createVerifyEth code) step
  checkResult c1 0 "Write succeeded"
  checkResult c3 1 "unsupported SPV types: ETH"

rSuccessTXOUT :: Text
rSuccessTXOUT = "TXOUT Success"

wrongChain :: (String -> IO ()) -> Assertion
wrongChain step = do
  (c1,c3) <- roundtrip 0 1 burnGen createWrongTargetChain step
  checkResult c1 0 "ObjectMap"
  checkResult c3 1 "Failure: enforceYield: yield provenance"

invalidProof :: (String -> IO ()) -> Assertion
invalidProof step = do
  (c1,c3) <- roundtrip 0 1 burnGen createInvalidProof step
  checkResult c1 0 "ObjectMap"
  checkResult c3 1 "Failure: resumePact: no previous execution found"

wrongChainProof :: (String -> IO ()) -> Assertion
wrongChainProof step = do
  (c1,c3) <- roundtrip 0 1 burnGen createProofBadTargetChain step
  checkResult c1 0 "ObjectMap"
  checkResult c3 1 "cannot redeem continuation proof on wrong target chain"
  return ()

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
    :: Word32
      -- ^ source chain id
    -> Word32
      -- ^ target chain id
    -> BurnGenerator
      -- ^ burn tx generator
    -> CreatesGenerator
      -- ^ create tx generator
    -> (String -> IO ())
    -> IO (CutOutputs, CutOutputs)
roundtrip = roundtrip' testVer

roundtrip'
    :: ChainwebVersion
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
roundtrip' v sid0 tid0 burn create step = withTestBlockDb v $ \bdb -> do
  tg <- newMVar mempty
  let logger = hunitDummyLogger step
  withWebPactExecutionService logger v testPactServiceConfig bdb (chainToMPA' tg) freeGasModel $ \(pact,_) -> do

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
    (BlockCreationTime t2) <- _blockCreationTime <$> getParentTestBlockDb bdb tid
    hi <- _blockHeight <$> getParentTestBlockDb bdb sid
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
    outs <- casLookupM pdb (_blockPayloadHash bh)
    let txs = Vector.map (toTx *** toCR) (_payloadWithOutputsTransactions outs)
        toTx :: Transaction -> Command Text
        toTx (Transaction t) = fromJuste $ decodeStrict' t
        toCR :: TransactionOutput -> CommandResult Hash
        toCR (TransactionOutput t) = fromJuste $ decodeStrict' t
    return txs

chainToMPA' :: MVar TransactionGenerator -> MemPoolAccess
chainToMPA' f = mempty
    { mpaGetBlock = \_g _pc hi ha he -> do
        tg <- readMVar f
        tg (_blockChainId he) hi ha he
    }


-- -------------------------------------------------------------------------- --
-- transaction generators

type TransactionGenerator
    = Chainweb.ChainId
    -> BlockHeight
    -> BlockHash
    -> BlockHeader
    -> IO (Vector ChainwebTransaction)

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
                cmd <- buildCwCmd v $
                  set cbSigners [mkEd25519Signer' sender00 []] $
                  set cbCreationTime (toTxCreationTime time) $
                  set cbChainId sid $
                  mkCmd "0" $
                  mkExec tx1Code tx1Data
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
                cmd <- buildCwCmd v $
                  set cbSigners
                    [mkEd25519Signer' sender00
                       [mkTransferCap "sender00" "sender01" 1.0
                       ,mkGasCap]] $
                  set cbCreationTime (toTxCreationTime time) $
                  set cbChainId sid $
                  -- FIXME what about the network id? It is Nothing
                  mkCmd "0" $
                  mkExec' tx1Code
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
  -> IO (Vector ChainwebTransaction)
createCont v cid pidv proof time = do
  pid <- readMVar pidv
  fmap Vector.singleton $
    buildCwCmd v $
    set cbSigners [mkEd25519Signer' sender00 []] $
    set cbCreationTime (toTxCreationTime time) $
    set cbChainId cid $
    mkCmd "1" $
    mkCont $
    ((mkContMsg pid 1) { _cmProof = proof })


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
                cmd <- buildCwCmd v $
                  set cbSigners [mkEd25519Signer' sender00 []] $
                  set cbCreationTime (toTxCreationTime time) $
                  set cbChainId tid $
                  mkCmd "0" $
                  mkExec
                    code
                    (object [("proof",q),("data",mdata)])
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
                cmd <- buildCwCmd v $
                  set cbSigners [mkEd25519Signer' sender00 []] $
                  set cbCreationTime (toTxCreationTime time) $
                  set cbChainId tid $
                  mkCmd "0" $
                  mkExec
                    code
                    (object [("proof", toJSON q)])
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

-- | Execute on the create-coin command on the wrong target chain
--
createWrongTargetChain :: CreatesGenerator
createWrongTargetChain v time (TestBlockDb wdb pdb _c) pidv sid tid bhe = do
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

                createCont v sid pidv proof time
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

-- | Execute on the create-coin command on the correct target chain, with a proof
-- pointing at the wrong target chain
--
createProofBadTargetChain :: CreatesGenerator
createProofBadTargetChain v time (TestBlockDb wdb pdb _c) pidv sid tid bhe = do
    ref <- newIORef False
    return $ go ref
  where
    go ref cid _bhe _bha _
        | tid /= cid = return mempty
        | otherwise = readIORef ref >>= \case
            True -> return mempty
            False -> do
                tid' <- chainIdFromText "2"
                q <- toJSON <$> createTransactionOutputProof_ wdb pdb tid' sid bhe 0

                let proof = Just . ContProof .  B64U.encode . toStrict . Aeson.encode $ q

                createCont v sid pidv proof time
                    `finally` writeIORef ref True
