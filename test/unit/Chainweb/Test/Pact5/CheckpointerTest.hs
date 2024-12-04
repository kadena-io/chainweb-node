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

module Chainweb.Test.Pact5.CheckpointerTest (tests) where

import Chainweb.BlockHeader
import Chainweb.Graph (singletonChainGraph)
import Chainweb.Logger
import Chainweb.MerkleLogHash
import Chainweb.MerkleUniverse (ChainwebMerkleHashAlgorithm)
import Chainweb.Pact.Types
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.Serialization (runGetS, runPutS)
import Chainweb.Version
import Control.Exception (evaluate)
import Control.Exception.Safe
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Functor.Product
import qualified Data.Map as Map
import Data.MerkleLog (MerkleNodeType(..), merkleRoot, merkleTree)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Hedgehog hiding (Update)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Numeric.AffineSpace
import Pact.Core.Builtin
import Pact.Core.Evaluate (Info)
import Pact.Core.Literal
import Pact.Core.Names
import qualified Pact.Core.PactDbRegression as Pact.Core
import Pact.Core.PactValue
import Pact.Core.Persistence
import qualified Streaming.Prelude as Stream
import Test.Tasty
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Hedgehog
import Chainweb.Test.Pact5.Utils
import Chainweb.Pact5.Backend.ChainwebPactDb (Pact5Db(doPact5DbTransaction))
import Chainweb.Pact5.Types (noInfo)
import GHC.Stack
import Chainweb.Pact.Backend.Types
import qualified Chainweb.Pact.PactService.Checkpointer.Internal as Checkpointer

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
genDbBlock = Gen.list (Range.linear 1 20) genDbAction

genBlockHistory :: Gen [DbBlock (Const ())]
genBlockHistory = do
    let create tn = DbCreateTable tn (Const ())
    blocks <- Gen.list (Range.constant 1 20) genDbBlock
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
    (fromException -> Just (PactInternalError _ text)) -> return text
    e -> return $ sshow e
    ) . fmap Right

runDbAction :: PactDb CoreBuiltin Info -> DbAction (Const ()) -> IO (DbAction Identity)
runDbAction pactDB act =
    fmap (hoistDbAction (\(Pair (Const ()) fa) -> fa))
        $ runDbAction' pactDB act

extractInt :: RowData -> IO Integer
extractInt (RowData m) = evaluate (m ^?! ix (Field "k") . _PLiteral . _LInteger)

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
blockHeaderFromTxLogs :: ParentHeader -> [TxLog ByteString] -> IO BlockHeader
blockHeaderFromTxLogs ph txLogs = do
    let
        logMerkleTree = merkleTree @ChainwebMerkleHashAlgorithm @ByteString
            [ TreeNode $ merkleRoot $ merkleTree
                [ InputNode (T.encodeUtf8 (_txDomain txLog))
                , InputNode (T.encodeUtf8 (_txKey txLog))
                , InputNode (_txValue txLog)
                ]
            | txLog <- txLogs
            ]
        encodedLogRoot = runPutS $ encodeMerkleLogHash $ MerkleLogHash $ merkleRoot logMerkleTree
    fakePayloadHash <- runGetS decodeBlockPayloadHash encodedLogRoot
    return $ newBlockHeader
        mempty
        fakePayloadHash
        (Nonce 0)
        (view blockCreationTime (_parentHeader ph) .+^ TimeSpan (1_000_000 :: Micros))
        ph

-- TODO things to test later:
-- that a tree of blocks can be explored, such that reaching any particular block gives identical results to running to that block from genesis

runBlocks
    :: Checkpointer GenericLogger
    -> ParentHeader
    -> [DbBlock (Const ())]
    -> IO [(BlockHeader, DbBlock Identity)]
runBlocks cp ph blks = do
    ((), finishedBlks) <- Checkpointer.restoreAndSave cp (Just ph) $ traverse_ Stream.yield
        [ Pact5RunnableBlock $ \db _ph startHandle -> do
            doPact5DbTransaction db startHandle Nothing $ \txdb -> do
                _ <- ignoreGas noInfo $ _pdbBeginTx txdb Transactional
                blk' <- traverse (runDbAction txdb) blk
                txLogs <- ignoreGas noInfo $ _pdbCommitTx txdb
                bh <- blockHeaderFromTxLogs (fromJuste _ph) txLogs
                return ([(bh, blk')], bh)
        | blk <- blks
        ]
    return finishedBlks

assertBlock :: HasCallStack => Checkpointer GenericLogger -> ParentHeader -> (BlockHeader, DbBlock Identity) -> IO ()
assertBlock cp ph (expectedBh, blk) = do
    hist <- Checkpointer.readFrom cp (Just ph) Pact5T $ \db startHandle -> do
        ((), _endHandle) <- doPact5DbTransaction db startHandle Nothing $ \txdb -> do
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

            actualBh <- blockHeaderFromTxLogs ph txLogs
            assertEqual "block header" expectedBh actualBh
        return ()
    throwIfNoHistory hist

tests :: TestTree
tests = testGroup "Pact5 Checkpointer tests"
    [ withResourceT (liftIO . initCheckpointer testVer cid =<< withTempSQLiteResource) $ \cpIO ->
        testCase "valid PactDb before genesis" $ do
            cp <- cpIO
            ((), _handle) <- (throwIfNoHistory =<<) $
                Checkpointer.readFrom cp Nothing Pact5T $ \db blockHandle -> do
                    doPact5DbTransaction db blockHandle Nothing $ \txdb ->
                        Pact.Core.runPactDbRegression txdb
            return ()
    , withResourceT (liftIO . initCheckpointer testVer cid =<< withTempSQLiteResource) $ \cpIO ->
        testProperty "linear block history validity" $ withTests 1000 $ property $ do
            blocks <- forAll genBlockHistory
            liftIO $ do
                cp <- cpIO
                -- extend this empty chain with the genesis block
                ((), ()) <- Checkpointer.restoreAndSave cp Nothing $ Stream.yield $ Pact5RunnableBlock $ \_ _ hndl ->
                    return (((), gh), hndl)
                -- run all of the generated blocks
                finishedBlocks <- runBlocks cp (ParentHeader gh) blocks
                let
                    finishedBlocksWithParents =
                        zip (fmap ParentHeader $ gh : (fst <$> finishedBlocks)) finishedBlocks
                -- assert that using readFrom to read from a parent, then executing the same block,
                -- gives the same results
                forM_ finishedBlocksWithParents $ \(ph, block) -> do
                    assertBlock cp ph block
    ]

testVer :: ChainwebVersion
testVer = pact5CheckpointerTestVersion singletonChainGraph

cid :: ChainId
cid = unsafeChainId 0

gh :: BlockHeader
gh = genesisBlockHeader testVer cid

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
