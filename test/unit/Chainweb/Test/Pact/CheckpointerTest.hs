{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Chainweb.Test.Pact.CheckpointerTest (tests) where

import Control.Exception (evaluate)
import Control.Exception.Safe
import Control.Lens
import Control.Monad
import Data.ByteString (ByteString)
import Data.Functor.Product
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.MerkleLog (MerkleNodeType (..), merkleRoot)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Hedgehog hiding (Update)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Pact.Core.Builtin
import Pact.Core.Evaluate (Info)
import Pact.Core.Literal
import Pact.Core.Names
import Pact.Core.PactDbRegression qualified as Pact.Core
import Pact.Core.PactValue
import Pact.Core.Persistence
import PropertyMatchers qualified as P
import Test.Tasty
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Hedgehog

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Graph (singletonChainGraph)
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse (ChainwebMerkleHashAlgorithm)
import Chainweb.Pact.Backend.ChainwebPactDb qualified as ChainwebPactDb
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.PactService.Checkpointer qualified as Checkpointer
import Chainweb.Pact.Types
import Chainweb.Parent
import Chainweb.PayloadProvider
import Chainweb.Test.Pact.Utils
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils hiding (withTempSQLiteResource)
import Chainweb.Utils
import Chainweb.Utils.Serialization (runGetS, runPutS)
import Chainweb.Version
import Chainweb.MinerReward
import Control.Monad.State.Strict
import qualified Chainweb.Payload as Chainweb
import Chainweb.Pact.Utils (emptyPayload)

-- | A @DbAction f@ is a description of some action on the database together with an f-full of results for it.
type DbValue = Integer
data DbAction f
    = DbRead !T.Text RowKey (f (Either Text (Maybe DbValue)))
    | DbWrite WriteType !T.Text RowKey DbValue (f (Either Text ()))
    | DbKeys !T.Text (f (Either Text [RowKey]))
    | DbSelect !T.Text (f (Either Text [(RowKey, Integer)]))
    | DbCreateTable T.Text (f (Either Text ()))

mkTableName :: T.Text -> TableName
mkTableName n = TableName n (ModuleName "mod" Nothing)

genDbAction :: Gen (DbAction (Const ()))
genDbAction = do
    let tn = Gen.choice [pure "A", pure "B", pure "C"]
    Gen.choice
        [ DbRead
            <$> tn
            <*> Gen.choice (pure . RowKey <$> ["A", "B", "C"])
            <*> pure (Const ())
        , DbWrite
            <$> genWriteType
            <*> tn
            <*> Gen.choice (pure . RowKey <$> ["A", "B", "C"])
            <*> fmap fromIntegral (Gen.int (Range.constant 0 5))
            <*> pure (Const ())
        , DbKeys <$> tn <*> pure (Const ())
        , DbSelect <$> tn <*> pure (Const ())
        , DbCreateTable <$> tn <*> pure (Const ())
        ]
    where
    genWriteType = Gen.choice $ fmap pure
        [ Write
        , Insert
        , Update
        ]

-- a block is a list of actions
type DbBlock f = [DbAction f]

genDbBlock :: Gen (DbBlock (Const ()))
genDbBlock = Gen.list (Range.constant 1 20) genDbAction

genBlockHistory :: Gen [DbBlock (Const ())]
genBlockHistory = do
    let create tn = DbCreateTable tn (Const ())
    blocks <- Gen.list (Range.linear 1 20) genDbBlock
    -- we always start by making tables A and B to ensure the tests do something,
    -- but we leave table C uncreated to leave some room for divergent table sets
    return $ [create "A", create "B"] : blocks

hoistDbAction :: (forall a. (Eq a, Show a) => f a -> g a) -> DbAction f -> DbAction g
hoistDbAction f (DbRead tn k r) = DbRead tn k (f r)
hoistDbAction f (DbWrite wt tn k v r) = DbWrite wt tn k v (f r)
hoistDbAction f (DbKeys tn ks) = DbKeys tn (f ks)
hoistDbAction f (DbSelect tn rs) = DbSelect tn (f rs)
hoistDbAction f (DbCreateTable tn es) = DbCreateTable tn (f es)

tryShow :: IO a -> IO (Either Text a)
tryShow = handleAny (fmap Left . \case
    e -> return (sshow e)
    ) . fmap Right

-- Run an empty DbAction, annotating it with its result
runDbAction :: PactDb CoreBuiltin Info -> DbAction (Const ()) -> IO (DbAction Identity)
runDbAction pactDB act =
    fmap (hoistDbAction (\(Pair (Const ()) fa) -> fa))
        $ runDbAction' pactDB act

extractInt :: RowData -> IO Integer
extractInt (RowData m) = evaluate (m ^?! ix (Field "k") . _PLiteral . _LInteger)

-- Annotate a DbAction with its result, including any other contents it has
runDbAction' :: PactDb CoreBuiltin Info -> DbAction f -> IO (DbAction (Product f Identity))
runDbAction' pactDB = \case
    DbRead tn k v -> do
        maybeValue <- tryShow $ ignoreGas noInfo $ _pdbRead pactDB (DUserTables (mkTableName tn)) k
        integerValue <- (traverse . traverse) extractInt maybeValue
        return $ DbRead tn k $ Pair v (Identity integerValue)
    DbWrite wt tn k v s ->
        fmap (DbWrite wt tn k v . Pair s . Identity)
            $ tryShow $ ignoreGas noInfo
            $ _pdbWrite pactDB wt (DUserTables (mkTableName tn)) k (RowData $ Map.singleton (Field "k") $ PLiteral $ LInteger v)
    DbKeys tn ks ->
        fmap (DbKeys tn . Pair ks . Identity)
            $ tryShow $ ignoreGas noInfo $ _pdbKeys pactDB (DUserTables (mkTableName tn))
    DbSelect tn rs ->
        fmap (DbSelect tn . Pair rs . Identity)
            $ tryShow $ do
                ks <- ignoreGas noInfo $ _pdbKeys pactDB (DUserTables (mkTableName tn))
                traverse (\k -> fmap (k,) . extractInt . fromJuste =<< ignoreGas noInfo (_pdbRead pactDB (DUserTables (mkTableName tn)) k)) ks
    DbCreateTable tn s ->
        fmap (DbCreateTable tn . Pair s . Identity)
            $ tryShow (ignoreGas noInfo $ _pdbCreateUserTable pactDB (mkTableName tn))

-- craft a fake block header from txlogs, i.e. some set of writes.
-- that way, the block header changes if the write set stops agreeing.
blockHeaderFromTxLogs :: Parent RankedBlockHash -> [TxLog ByteString] -> IO (BlockHash, BlockPayloadHash)
blockHeaderFromTxLogs parent txLogs = do
    fakePayloadHash <- runGetS decodeBlockPayloadHash $
        let
            payloadLogMerkleTree = merkleRoot @ChainwebMerkleHashAlgorithm
                [ TreeNode $ merkleRoot $
                    [ InputNode (T.encodeUtf8 (_txDomain txLog))
                    , InputNode (T.encodeUtf8 (_txKey txLog))
                    , InputNode (_txValue txLog)
                    ]
                | txLog <- txLogs
                ]
        in
            runPutS $ encodeMerkleLogHash $ MerkleLogHash payloadLogMerkleTree
    fakeBlockHash <- runGetS decodeBlockHash $
        let
            blockLogMerkleTree = merkleRoot @ChainwebMerkleHashAlgorithm
                [ TreeNode $ merkleRoot $
                    [ InputNode (runPutS $ encodeBlockHash $ unwrapParent $ _rankedBlockHashHash <$> parent)
                    , InputNode (runPutS $ encodeBlockPayloadHash @ChainwebMerkleHashAlgorithm fakePayloadHash)
                    ]
                ]
        in
            runPutS $ encodeMerkleLogHash $ MerkleLogHash blockLogMerkleTree
    return (fakeBlockHash, fakePayloadHash)

-- TODO things to test later:
-- that a tree of blocks can be explored, such that reaching any particular block gives identical results to running to that block from genesis
-- more specific regressions, like in the Pact 4 checkpointer test

runBlocks
    :: SQLiteEnv
    -> Parent RankedBlockHash
    -> [DbBlock (Const ())]
    -> IO [(BlockCtx, (BlockHash, BlockPayloadHash), DbBlock Identity)]
runBlocks sql rootBlockCtx blks =
    loop rootBlockCtx blks
    where
    loop parent (block:blocks) = withVersion testVer $ do
        logger <- getTestLogger
        fakeParentCreationTime <- Checkpointer.mkFakeParentCreationTime
        (fakeBlockInfo, block', _finalBlockHandle) <-
            (throwIfNoHistory =<<) $
                Checkpointer.readFrom logger cid sql fakeParentCreationTime parent $
                    executeBlockTransaction parent block
        let childBlockCtx = BlockCtx
                { _bctxParentCreationTime = fakeParentCreationTime
                , _bctxParentHash = Parent $ fst fakeBlockInfo
                , _bctxParentHeight = Parent $ childBlockHeight cid parent
                , _bctxChainId = cid
                , _bctxMinerReward = blockMinerReward (childBlockHeight cid parent)
                }
        let parentBlockCtx = BlockCtx
                { _bctxParentCreationTime = fakeParentCreationTime
                , _bctxParentHash = _rankedBlockHashHash <$> parent
                , _bctxParentHeight = _rankedBlockHashHeight <$> parent
                , _bctxChainId = cid
                , _bctxMinerReward = blockMinerReward (unwrapParent $ _rankedBlockHashHeight <$> parent)
                }
        _ <- Checkpointer.restoreAndSave logger cid sql
            (NE.singleton (parentBlockCtx, \blockEnv -> do
                blockHandle <- get
                (fakeBlockInfo', _blk, finalBlockHandle) <-
                    liftIO $ executeBlockTransaction parent block blockEnv blockHandle
                put finalBlockHandle
                liftIO $ fakeBlockInfo' & P.equals fakeBlockInfo
                return ((), fakeBlockInfo)
                ))
        ((parentBlockCtx, fakeBlockInfo, block') :) <$> loop (_bctxParentRankedBlockHash childBlockCtx) blocks
    loop _ [] = return []
    executeBlockTransaction parent block blockEnv blockHandle = do
        ((childRankedBlockHash, blk'), finalBlockHandle) <- doChainwebPactDbTransaction (_psBlockDbEnv blockEnv) blockHandle Nothing $ \txdb _spv -> do
            _ <- ignoreGas noInfo $ _pdbBeginTx txdb Transactional
            blk' <- traverse (runDbAction txdb) block
            txLogs <- ignoreGas noInfo $ _pdbCommitTx txdb
            blockInfo <- blockHeaderFromTxLogs parent txLogs
            return (blockInfo, blk')
        return (childRankedBlockHash, blk', finalBlockHandle)

-- Check that a block's result at the time it was added to the checkpointer
-- is consistent with us executing that block with `readFrom`
assertBlock :: SQLiteEnv -> BlockCtx -> (BlockHash, BlockPayloadHash) -> DbBlock Identity -> IO ()
assertBlock sql blockCtx expectedBlockInfo blk = withVersion testVer $ do
    fakeNewBlockCtx <- Checkpointer.mkFakeParentCreationTime
    logger <- getTestLogger
    hist <- Checkpointer.readFrom logger cid sql fakeNewBlockCtx (_bctxParentRankedBlockHash blockCtx) $ \blockEnv startHandle -> do
        ((), _endHandle) <- doChainwebPactDbTransaction (_psBlockDbEnv blockEnv) startHandle Nothing $ \txdb _spv -> do
            _ <- ignoreGas noInfo $ _pdbBeginTx txdb Transactional
            blk' <- forM blk (runDbAction' txdb)
            txLogs <- ignoreGas noInfo $ _pdbCommitTx txdb
            forM_ blk' $ \case
                DbRead _d _k (Pair expected actual) ->
                    assertEqual "read result" expected actual
                DbWrite _wt _d _k _v (Pair expected actual) ->
                    assertEqual "write result" expected actual
                DbKeys _d (Pair expected actual) ->
                    assertEqual "keys result" expected actual
                DbSelect _d (Pair expected actual) ->
                    assertEqual "select result" expected actual
                DbCreateTable _tn (Pair expected actual) ->
                    assertEqual "create table result" expected actual

            actualBlockInfo <-
                blockHeaderFromTxLogs (_bctxParentRankedBlockHash blockCtx) txLogs
            assertEqual "block header" expectedBlockInfo actualBlockInfo
        return ()
    throwIfNoHistory hist

tests :: TestTree
tests = testGroup "Pact5 Checkpointer tests"
    [ withResourceT (withTempChainSqlite cid) $ \sqlIO ->
        testCase "valid PactDb before genesis" $ withVersion testVer $ do
            (sql, _sqlReadPool) <- sqlIO
            ChainwebPactDb.initSchema sql
            Checkpointer.setConsensusState sql $ genesisConsensusState cid
            logger <- getTestLogger
            fakeNewBlockCtx <- Checkpointer.mkFakeParentCreationTime
            ((), _handle) <- (throwIfNoHistory =<<) $
                Checkpointer.readFrom logger cid sql fakeNewBlockCtx genesisParentRanked
                    $ \db blockHandle -> do
                    doChainwebPactDbTransaction (_psBlockDbEnv db) blockHandle Nothing $ \txdb _spv ->
                        Pact.Core.runPactDbRegression txdb
            return ()
    , withResourceT (withTempChainSqlite cid) $ \sqlIO ->
        testProperty "readFrom with linear block history is valid" $ withTests 1000 $ property $ withVersion testVer $ do
            blocks <- forAll genBlockHistory
            (sql, _sqlReadPool) <- evalIO sqlIO
            finishedBlocks <- evalIO $ do
                ChainwebPactDb.initSchema sql
                Checkpointer.setConsensusState sql $ genesisConsensusState cid
                logger <- getTestLogger
                -- extend this empty chain with the genesis block
                _ <- Checkpointer.restoreAndSave logger cid sql $
                    (
                        NE.singleton (blockCtxOfEvaluationCtx cid (genesisEvalCtx cid),
                        \_ -> return ((), (view blockHash (genesisBlockHeader cid), genesisBlockPayloadHash cid)))
                    )
                handle @_ @SomeException
                    (\ex -> putStrLn (displayException ex) >> throw ex)
                    (runBlocks sql (Parent $ view rankedBlockHash gh) blocks)
                -- run all of the generated blocks
            annotateShow finishedBlocks
            -- assert that using readFrom to read from a parent, then executing the same block,
            -- gives the same results
            evalIO $ forM_ finishedBlocks $ \(parent, blockInfo, block) -> do
                assertBlock sql parent blockInfo block
    ]
    where
    genesisEvalCtx c = withVersion testVer $ EvaluationCtx
        { _evaluationCtxParentCreationTime = Parent $ implicitVersion ^?! versionGenesis . genesisTime . atChain c
        , _evaluationCtxParentHash = genesisParentBlockHash c
        , _evaluationCtxParentHeight = Parent $ genesisHeight c
        -- should not be used
        , _evaluationCtxMinerReward = MinerReward 0
        , _evaluationCtxPayload = ConsensusPayload
            { _consensusPayloadHash = genesisBlockPayloadHash c
            , _consensusPayloadData = Just $ EncodedPayloadData $ Chainweb.encodePayloadData $
                Chainweb.payloadWithOutputsToPayloadData emptyPayload
            }
        }


testVer :: ChainwebVersion
testVer = checkpointerTestVersion singletonChainGraph

cid :: ChainId
cid = unsafeChainId 0

gh :: BlockHeader
gh = withVersion testVer $ genesisBlockHeader cid

genesisParentRanked :: Parent RankedBlockHash
genesisParentRanked = withVersion testVer $
    Parent $ RankedBlockHash
        (genesisHeight cid)
        (unwrapParent $ genesisParentBlockHash cid)

instance (forall a. Show a => Show (f a)) => Show (DbAction f) where
    showsPrec n (DbRead tn k v) = showParen (n > 10) $
        showString "DbRead " . showsPrec 11 tn
        . showString " " . showsPrec 11 k
        . showString " " . showsPrec 11 v
    showsPrec n (DbWrite wt tn k v r) = showParen (n > 10) $
        showString "DbWrite " . showsPrec 11 wt
        . showString " " . showsPrec 11 tn
        . showString " " . showsPrec 11 k
        . showString " " . showsPrec 11 v
        . showString " " . showsPrec 11 r
    showsPrec n (DbKeys tn ks) = showParen (n > 10) $
        showString "DbKeys " . showsPrec 11 tn
        . showString " " . showsPrec 11 ks
    showsPrec n (DbSelect tn rs) = showParen (n > 10) $
        showString "DbSelect " . showsPrec 11 tn
        . showString " " . showsPrec 11 rs
    showsPrec n (DbCreateTable tn r) = showParen (n > 10) $
        showString "DbSelect " . showsPrec 11 tn
        . showString " " . showsPrec 11 r
