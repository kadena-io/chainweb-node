{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Control.Lens (set)

import Data.Aeson as Aeson
import qualified Data.ByteString.Base64.URL as B64U
import Data.ByteString.Lazy (toStrict)
import Data.CAS (casLookupM)
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.List (isInfixOf)
import Data.Text (pack,Text)
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word

import NeatInterpolation (text)

import System.LogLevel

import Test.Tasty
import Test.Tasty.HUnit

-- internal pact modules

import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.Runtime (toPactId)
import Pact.Types.SPV
import Pact.Types.Term


-- internal chainweb modules

import Chainweb.BlockCreationTime
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Cut
import Chainweb.Graph
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.Payload
import Chainweb.Payload.PayloadStore
import Chainweb.SPV.CreateProof
import Chainweb.Test.Cut
import Chainweb.Test.Cut.TestBlockDb
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Transaction
import Chainweb.Utils hiding (check)
import Chainweb.Version as Chainweb
import Chainweb.WebPactExecutionService

import Data.LogMessage


-- | Note: These tests are intermittently non-deterministic due to the way
-- random chain sampling works with our test harnesses.
--
tests :: TestTree
tests = testGroup "Chainweb.Test.Pact.SPV"
    [ testCaseSteps "standard SPV verification round trip" standard
    , testCaseSteps "wrong chain execution fails" wrongChain
    , testCaseSteps "invalid proof formats fail" invalidProof
    , testCaseSteps "wrong target chain in proofs fail" wrongChainProof
    ]

v :: ChainwebVersion
v = FastTimedCPM triangleChainGraph

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

standard :: (String -> IO ()) -> Assertion
standard step = do
  (c1,c3) <- roundtrip 0 1 burnGen createSuccess step
  checkResult c1 0 "ObjectMap"
  checkResult c3 1 "Write succeeded"


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


withAll :: ChainwebVersion -> ([SQLiteEnv] -> IO c) -> IO c
withAll vv f = foldl' (\soFar _ -> with soFar) f (chainIds vv) []
  where
    with :: ([SQLiteEnv] -> IO c) -> [SQLiteEnv] -> IO c
    with g envs =  withTempSQLiteConnection chainwebPragmas $ \s -> g (s : envs)

getCutOutputs :: TestBlockDb -> IO CutOutputs
getCutOutputs (TestBlockDb _ pdb cmv) = do
  c <- readMVar cmv
  cutToPayloadOutputs c pdb

-- | Populate blocks for every chain of the current cut. Uses provided pact
-- service to produce a new block, add it
runCut' :: TestBlockDb -> WebPactExecutionService -> IO CutOutputs
runCut' bdb pact = do
  runCut v bdb pact (offsetBlockTime second) zeroNoncer
  getCutOutputs bdb


roundtrip
    :: Int
      -- ^ source chain id
    -> Int
      -- ^ target chain id
    -> BurnGenerator
      -- ^ burn tx generator
    -> CreatesGenerator
      -- ^ create tx generator
    -> (String -> IO ())
    -> IO (CutOutputs, CutOutputs)
roundtrip sid0 tid0 burn create step = do
  withTestBlockDb v $ \bdb@(TestBlockDb wdb pdb _) -> withAll v $ \sqlenvs -> do

    sid <- mkChainId v sid0
    tid <- mkChainId v tid0

    -- track the continuation pact id
    pidv <- newEmptyMVar @PactId

    tg <- newMVar mempty
    pact <- testWebPactExecutionService v
              (return wdb) (return pdb)
              (chainToMPA' tg) sqlenvs

    -- cut 0: empty run (not sure why this is needed but test fails without it)
    step "cut 0: empty run"
    void $ runCut' bdb pact

    -- cut 1: burn
    step "cut 1: burn"
    (BlockCreationTime t1) <- _blockCreationTime <$> getParentTestBlockDb bdb sid
    txGen1 <- burn t1 pidv sid tid
    void $ swapMVar tg txGen1
    co1 <- runCut' bdb pact

    -- setup create txgen with cut 1
    step "setup create txgen with cut 1"
    (BlockCreationTime t2) <- _blockCreationTime <$> getParentTestBlockDb bdb tid
    hi <- _blockHeight <$> getParentTestBlockDb bdb sid
    txGen2 <- create t2 bdb pidv sid tid hi

    -- cut 2: empty cut for diameter 1
    step "cut 2: empty cut for diameter 1"
    void $ swapMVar tg mempty
    void $ runCut' bdb pact

    -- cut 3: create
    step "cut 3: create"
    void $ swapMVar tg txGen2
    co2 <- runCut' bdb pact

    return (co1,co2)


_debugCut :: PayloadCasLookup cas => String -> Cut -> PayloadDb cas -> IO ()
_debugCut msg c pdb = do
  putStrLn $ "CUT: =============== " ++ msg
  outs <- cutToPayloadOutputs c pdb
  forM_ (HM.toList outs) $ \(cid,vs) -> do
    putStrLn $ "Chain: " ++ show cid
    forM_ vs $ \(cmd,pr) -> do
      putStrLn $ show (_cmdHash cmd) ++ ": " ++ show pr

type CutOutputs = HM.HashMap Chainweb.ChainId (Vector (Command Text, CommandResult Hash))

cutToPayloadOutputs
  :: PayloadCasLookup cas
  => Cut
  -> PayloadDb cas
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

chainToMPA' :: MVar TransactionGenerator -> Chainweb.ChainId -> MemPoolAccess
chainToMPA' f cid = mempty
    { mpaGetBlock = \_pc hi ha he -> do
        tg <- readMVar f
        tg cid hi ha he
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
    = Time Micros -> MVar PactId -> Chainweb.ChainId -> Chainweb.ChainId -> IO TransactionGenerator

type CreatesGenerator
    = Time Micros
    -> TestBlockDb
    -> MVar PactId
    -> Chainweb.ChainId
    -> Chainweb.ChainId
    -> BlockHeight
    -> IO TransactionGenerator

-- | Generate burn/create Pact Service commands on arbitrarily many chains
--
burnGen :: BurnGenerator
burnGen time pidv sid tid = do
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
                cmd <- buildCwCmd $
                  set cbSigners [mkSigner' sender00 []] $
                  set cbCreationTime (toTxCreationTime time) $
                  set cbChainId sid $
                  mkCmd "0" $
                  mkExec tx1Code tx1Data
                writeIORef ref0 True

                let pid = toPactId $ toUntypedHash $ _cmdHash cmd

                putMVar pidv pid `finally` writeIORef ref1 True
                return $ Vector.singleton cmd



    tx1Code =
      [text|
        (coin.transfer-crosschain
          'sender00
          'sender01
          (read-keyset 'sender01-keyset)
          (read-msg 'target-chain-id)
          1.0)
        |]

    tx1Data =
      -- sender01 keyset guard
      let ks = mkKeySet
            ["6be2f485a7af75fedb4b7f153a903f7e6000ca4aa501179c91a2450b777bd2a7"]
            "keys-all"

      in object
         [ "sender01-keyset" .= ks
         , "target-chain-id" .= chainIdToText tid
         ]

createCont
  :: ChainId
     -> MVar PactId
     -> Maybe ContProof
     -> Time Micros
     -> IO (Vector ChainwebTransaction)
createCont cid pidv proof time = do
  pid <- readMVar pidv
  fmap Vector.singleton $
    buildCwCmd $
    set cbSigners [mkSigner' sender00 []] $
    set cbCreationTime (toTxCreationTime time) $
    set cbChainId cid $
    mkCmd "1" $
    mkCont $
    ((mkContMsg pid 1) { _cmProof = proof })

-- | Generate the 'create-coin' command in response to the previous 'delete-coin' call.
-- Note that we maintain an atomic update to make sure that if a given chain id
-- has already called the 'create-coin' half of the transaction, it will not do so again.
--
createSuccess :: CreatesGenerator
createSuccess time (TestBlockDb wdb pdb _c) pidv sid tid bhe = do
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
                createCont tid pidv proof time
                    `finally` writeIORef ref True

-- | Execute on the create-coin command on the wrong target chain
--
createWrongTargetChain :: CreatesGenerator
createWrongTargetChain time (TestBlockDb wdb pdb _c) pidv sid tid bhe = do
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

                createCont sid pidv proof time
                    `finally` writeIORef ref True

-- | Execute create-coin command with invalid proof
--
createInvalidProof :: CreatesGenerator
createInvalidProof time _ pidv _ tid _ = do
    ref <- newIORef False
    return $ go ref
  where
    go ref cid _bhe _bha _
        | tid /= cid = return mempty
        | otherwise = readIORef ref >>= \case
            True -> return mempty
            False ->
                createCont tid pidv Nothing time
                    `finally` writeIORef ref True

-- | Execute on the create-coin command on the correct target chain, with a proof
-- pointing at the wrong target chain
--
createProofBadTargetChain :: CreatesGenerator
createProofBadTargetChain time (TestBlockDb wdb pdb _c) pidv sid tid bhe = do
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

                createCont sid pidv proof time
                    `finally` writeIORef ref True
