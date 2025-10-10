{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Chainweb.Test.Pact.BlockHistoryMigrationTest
  (tests)
where

import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.BlockHeaderDB
import Chainweb.BlockHeaderDB.Internal
import Chainweb.Logger
import Chainweb.Pact.Backend.ChainwebPactDb qualified as ChainwebPactDb
import Chainweb.Pact.Backend.PactState
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils
import Chainweb.PayloadProvider.Pact.BlockHistoryMigration (migrateBlockHistoryTable)
import Chainweb.Storage.Table.RocksDB
import Chainweb.Test.Pact.Utils
import Chainweb.Test.Utils
import Chainweb.TreeDB
import Chainweb.Utils
import Chainweb.Utils.Serialization
import Chainweb.Version
import Chainweb.Version.Mainnet
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Aeson (throwDecodeStrict')
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Database.SQLite3.Direct
import Streaming.Prelude qualified as S
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)

cid :: ChainId
cid = unsafeChainId 0

createLegacyBlockHistoryTable :: SQLiteEnv -> IO ()
createLegacyBlockHistoryTable sql = throwOnDbError $
  exec_ sql
    "CREATE TABLE IF NOT EXISTS BlockHistory \
    \(blockheight UNSIGNED BIGINT NOT NULL, \
    \ endingtxid UNSIGNED BIGINT NOT NULL, \
    \ hash BLOB NOT NULL, \
    \ CONSTRAINT blockHeightConstraint UNIQUE (blockheight), \
    \ CONSTRAINT hashConstraint UNIQUE (hash));"


initSchema :: SQLiteEnv -> IO ()
initSchema sql = do
  ChainwebPactDb.initSchema sql     -- create the BlockHistory2 table
  createLegacyBlockHistoryTable sql -- create the legacy BlockHistory table

withSetup
  :: TestName
  -> (SQLiteEnv -> IO ())
  -> (HasVersion => GenericLogger -> SQLiteEnv -> BlockHeaderDb -> Bool -> IO ())
  -> TestTree
withSetup n setup action = withResourceT (withTempChainSqlite cid) $ \sqlIO -> do
  testCase n $ do
    logger <- getTestLogger
    (sql, _sqlReadPool) <- sqlIO

    _ <- setup sql

    withTempRocksDb "chainweb-tests" $ \rdb -> do
      withVersion Mainnet01 $ runResourceT $ do
        bhdb <- withBlockHeaderDb rdb cid
        liftIO $ action logger sql bhdb True

tests :: HasCallStack => TestTree
tests = testGroup "BlockHistory Table Migration" [
     withSetup "test with empty BlockHistoryTable"
        initSchema
        migrateBlockHistoryTable
    , withSetup "test successful migration cleanup"
        initSchema
        $ \lf sdb bhdb cleanup -> do
            let qryIO = throwOnDbError $ qry sdb "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = 'BlockHistory'" [] [RInt]
            [[SInt p]] <- qryIO
            assertExpectation "Table should be present" (Expected 1) (Actual p)
            migrateBlockHistoryTable lf sdb bhdb cleanup
            post <- qryIO
            assertExpectation "Table should not be present" (Expected []) (Actual post)
    , withSetup "test migration"
        initSchema
        $ \lf sdb bhdb _cleanup -> do
          traverse_ (unsafeInsertBlockHeaderDb bhdb) blockHeaders
          traverse_ (unsafeInsertEntry sdb) sqliteData

          -- Disable original table cleanup for migration verification.
          migrateBlockHistoryTable lf sdb bhdb False

          verifyMigration sdb bhdb

          -- Re-run verification
          migrateBlockHistoryTable lf sdb bhdb False

          verifyMigration sdb bhdb
    , withSetup "test migration with one missing row"
        initSchema
        $ \lf sdb bhdb _cleanup -> do
          traverse_ (unsafeInsertBlockHeaderDb bhdb) blockHeaders
          traverse_ (unsafeInsertEntry sdb) sqliteData

          -- Disable original table cleanup for migration verification.
          migrateBlockHistoryTable lf sdb bhdb False

          verifyMigration sdb bhdb

          -- remove single row from BlockHistory2 table
          let (rbh,_,_) = head sqliteData
          throwOnDbError $ exec' sdb "DELETE FROM BlockHistory2 where blockheight=?" [SInt rbh]

          n <- nTableEntries sdb "BlockHistory2"
          assert (n == 9) $ "BlockHistory2 should contain 9 entries, actual: " <> sshow n

          -- Re-run verification
          migrateBlockHistoryTable lf sdb bhdb False

          verifyMigration sdb bhdb
    ]


-- | Blockheader from mainnet01 of chain 1
--
-- Obtained by curl -H 'https://api.chainweb.com/chainweb/0.0/mainnet01/chain/1/header?limit=10' | jq
blockHeaders :: [BlockHeader]
blockHeaders = fromJust $ traverse throwDecodeStrict'  [
    "\"AAAAAAAAAAAAJ41tFZYFAMhy366rCocPqnIPH492fe3J_cMShzHQwiyBVMT5RnKFAwADAAAAUsNRCUkMKQBRDVIv0JLDfDllYNg2QO0EXaWcRfMQ6g0EAAAAOu6kxMXpuwpm9uiIVhiEFHSbTdhanjHyiEh2G0EZV_wGAAAABQeojaqG63-R_1bILrd0LI6kDkS74hNBB0Bs3Y211bD__________________________________________5Apaf08O5Hgi1zH2gsox_oE6ABpi70UKojUZoVlAm8bAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAFAAAAACeNbRWWBQAAAAAAAAAAAGlRRahfD2xrvjGZFIW9TpOF4o4MVlGMQcYoAUIGZgxS\"",
    "\"jw3vOXLHNX9X8Pf8F5YFAGlRRahfD2xrvjGZFIW9TpOF4o4MVlGMQcYoAUIGZgxSAwADAAAAtMyy7k6lrQOsGg2oXh20kOC3zudjwsaL9F5Ogx0XNBoEAAAAyeWKDE27xM45-LgdwpnVMM6E4pAoYryRrJJFgRiaKgQGAAAAOXehM2W5YuiV0iWlDGIPMdaRv721zMUbj3caJqZJNBX__________________________________________3CgdhATU9VNY_SB4Z9xQYmupqf0-0oZU5WKfk0I_SJJAQAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQAAAAAAAAAFAAAAACeNbRWWBQAAAAAAAAAAABwrUPRfEh4gKXiQaLRVoqUK1aDCluIWbmZQzt9lUSDV\"",
    "\"NSpftriT22gDWwz9F5YFABwrUPRfEh4gKXiQaLRVoqUK1aDCluIWbmZQzt9lUSDVAwADAAAAe0GGnclF0G2-_AMlCLGjKJVgzSBj1V9dTiqtsCC-IB4EAAAAuekHplt74MFfLHPsu7cJz9HbWNb_vBY-MR1y5RRDiYsGAAAA7OcrWyX2Gk7zrwmdI6HALWBvCihE6PwaS7TELscDGCH__________________________________________7sfFm8JbPK3qCTNeRvk4tzVrddPKuqFw-tI68WMfCvbAQAAAAIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgAAAAAAAAAFAAAAACeNbRWWBQAAAAAAAAAAALpshckMhpgcor3YL9kXfHzYpq1iMsbjLA-2QKABndRt\"",
    "\"h1RefsOyecrGlGX9F5YFALpshckMhpgcor3YL9kXfHzYpq1iMsbjLA-2QKABndRtAwADAAAAUYQRTGxyJpPIvFxR8mRa-wtGsBD9ymVwuPv9ta90DCEEAAAAhcayV2lxaq0ExtZZAKbHGptweEhk44dh3rk_6QGEff8GAAAAxu26E8QHPhi3GfZjOtisZyzMuR6E4-ekdfO-eWd06YT__________________________________________-nZJfKtvRk1CPK-MEaEfWAy0WrGA5g7b0xFyq1LykpHAQAAAAMAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAwAAAAAAAAAFAAAAACeNbRWWBQAAAAAAAAAAALO4obrG733x3raJyCjCQKWTp9IxoVDXC00thjQhnq5Z\"",
    "\"ApXp4xPFfLL6MHT9F5YFALO4obrG733x3raJyCjCQKWTp9IxoVDXC00thjQhnq5ZAwADAAAAKuCNwclDosE_LMTCJPR-m8cLSTlQodWupf1c7PacZMEEAAAAlO6bNRGAwyiMdOAcDBushxBQheVa6Ra7TAd6OECzVVUGAAAAY00AQMeyYamYt6RnqEmQkAJVSt_OyNTZPA8ySAbs1c___________________________________________7Rres8j0FBEYwtUcS5UBNQumgtf0PG6j7y4VRMafRdmAQAAAAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABAAAAAAAAAAFAAAAACeNbRWWBQAAAAAAAAAAAJr-V0iWi6yBDNGqEU0oJLNUrVOACJmITKsOvRz4H90R\"",
    "\"0t473zHYtI-uNbf9F5YFAJr-V0iWi6yBDNGqEU0oJLNUrVOACJmITKsOvRz4H90RAwADAAAAumaN584K3DrEDxwpyuETFMsQ6ECseJVda54NIeQn8TkEAAAAv9sZzSMjqRD_uVMGoM-5kVnNqkRTYGqH6O-C7dIqXJsGAAAAcsRGLYQHN1NIYa1sXlj0eFaYwDLG7Vg3zqps05VEvGj__________________________________________5-87RGWTTcz0_voXhENxoJO6YTSH0GpwPkzCAxBCSvwAQAAAAUAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABQAAAAAAAAAFAAAAACeNbRWWBQAAAAAAAAAAAERk0j0pZ1OduMRVwNDrexiPbxnTPTKIjwwNmsZ-gl6H\"",
    "\"vhmHzX6Tuzdm--H9F5YFAERk0j0pZ1OduMRVwNDrexiPbxnTPTKIjwwNmsZ-gl6HAwADAAAA7mB5gbJFX_TnYhstzsiLBEhe0kT5nhh_JpPHfe7gMMEEAAAAxC3P3K1B1gDjwyLJPABbGt2qLuF93Y7QyiTNctqm94cGAAAA8JQk7mo7jCppLZnaHG6Doz6dOxrrKLXypB8CdzuAKHX__________________________________________65nMrh2AwrQTKIvl7YVg3ulCXikvFzPX4dVpPT9-s6cAQAAAAYAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABgAAAAAAAAAFAAAAACeNbRWWBQAAAAAAAAAAAMpV6GV2nJeFmSVBjo7QzWvF5W2LPS9uKFUCKJfU29Qr\"",
    "\"XDW_QcCklR_DbUD-F5YFAMpV6GV2nJeFmSVBjo7QzWvF5W2LPS9uKFUCKJfU29QrAwADAAAAlk-iO8laTJ5Gy-U7pE-P3EDr548IUL1sWOfZv9DGHn0EAAAAhFuMKuqHuk4fA2BOBCTvTEFrOZEs691JLXuINP3BSu4GAAAA85F8Vdy_NKXopcOdG4pcEpAw8G2Xk4wWfp29d_T5xk7__________________________________________7g-M1b3FLNbXKogETFOeA6w2xPeMWVODCbJl8PHDEGDAQAAAAcAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABwAAAAAAAAAFAAAAACeNbRWWBQAAAAAAAAAAAF8yBC9hIAHcsIUtgH5-RFdC2-QAmkmuB93qDIlXLWBB\"",
    "\"415OTHBmkcV-oJn-F5YFAF8yBC9hIAHcsIUtgH5-RFdC2-QAmkmuB93qDIlXLWBBAwADAAAAPT37vtcbWdrZUi21FIrCyeUl4rU6g4W6syR3iM-mSyUEAAAAUcfuhJG94kMymAtpftR3Vbv3dNuCCpIOaf2PZULHemwGAAAAumhex2eCzZTd9mVjd8QBug13FN2Y_65X68Guel01bLb__________________________________________9lYTtyLATglm7r7WHyqgYL3tqtOdVjaYqlv2w6JpDmmAQAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAAAAAAFAAAAACeNbRWWBQAAAAAAAAAAAE8hMzYm6jX5iR2HwqCIreHN4eHJNBwlAxdUp0A4wEJ1\"",
    "\"uD82CpEfylAigrn-F5YFAE8hMzYm6jX5iR2HwqCIreHN4eHJNBwlAxdUp0A4wEJ1AwADAAAAB4vWZj8UZzuvPJkCq3-_32I8pCkdePIRlYz3UIudbowEAAAAFYSAHoaThaLUdIFRcFYSkNm3xk0s7nzDMfY-wB-hp5YGAAAAwT2FOsosNkjFLH4-l-WFevFkAnJiFU-8IpGqpYb0_tr__________________________________________18PgFBxcI7IqYgDsHrSRiwWiAcrodG0YQBKFQuhxxcZAQAAAAkAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACQAAAAAAAAAFAAAAACeNbRWWBQAAAAAAAAAAAO8gTCqlCDrGsVZkvavO_Cr6GX5y6iCcp7XvtXCSsyae\""
  ]

unsafeInsertEntry :: SQLiteEnv -> (Int64, BS.ByteString, Int64) -> IO ()
unsafeInsertEntry sql (bh, h, txid) = case B64.decode h of
  Right h' ->
    throwOnDbError $ exec' sql "INSERT INTO BlockHistory (blockheight, endingtxid, hash) VALUES (?, ?, ?)"
      [SInt bh, SInt txid, SBlob h' ]
  Left _ -> error "error decoding hash"


sqliteData :: [(Int64, BS.ByteString, Int64)]
sqliteData = [
  (0, "aVFFqF8PbGu+MZkUhb1Ok4XijgxWUYxBxigBQgZmDFI=", 6),
  (1, "HCtQ9F8SHiApeJBotFWipQrVoMKW4hZuZlDO32VRINU=", 7),
  (2, "umyFyQyGmByivdgv2Rd8fNimrWIyxuMsD7ZAoAGd1G0=", 8),
  (3, "s7ihusbvffHetonIKMJApZOn0jGhUNcLTS2GNCGerlk=", 9),
  (4, "mv5XSJaLrIEM0aoRTSgks1StU4AImYhMqw69HPgf3RE=", 10),
  (5, "RGTSPSlnU524xFXA0Ot7GI9vGdM9MoiPDA2axn6CXoc=", 11),
  (6, "ylXoZXacl4WZJUGOjtDNa8XlbYs9L24oVQIol9Tb1Cs=", 12),
  (7, "XzIEL2EgAdywhS2Afn5EV0Lb5ACaSa4H3eoMiVctYEE=", 13),
  (8, "TyEzNibqNfmJHYfCoIit4c3h4ck0HCUDF1SnQDjAQnU=", 14),
  (9, "7yBMKqUIOsaxVmS9q878KvoZfnLqIJynte+1cJKzJp4=", 15)]


verifyMigration :: HasVersion => SQLiteEnv -> BlockHeaderDb -> IO ()
verifyMigration sql bhdb = do
  let qstmt = "SELECT A.blockheight, A.endingtxid, \
                    \ B.hash AS b_hash, B.payloadhash AS b_payload_hash, \
                    \ A.hash AS a_hash \
                    \ FROM BlockHistory AS A INNER JOIN BlockHistory2 AS B \
                    \ ON A.blockheight = B.blockheight AND A.endingtxid = B.endingtxid \
                    \ ORDER BY A.blockheight, A.endingtxid"
      rty = [RInt, RInt, RBlob, RBlob, RBlob]

  n <- nTableEntries sql "BlockHistory"

  _ <- qryStream sql  qstmt [] rty $ \rs -> do
    rs & flip S.mapM_ $ \case
      [SInt a_bh, SInt a_etxid, SBlob b_hash, SBlob b_payload, SBlob a_hash] -> do
        assert (a_hash == b_hash) $
                "Hash mismatch at block " ++ show a_bh ++ " / txid " ++ show a_etxid

        let rowBlockHeight = fromIntegral a_bh
        rowBlockHash <- runGetS decodeBlockHash a_hash
        blockHeader <- lookupRankedM bhdb rowBlockHeight rowBlockHash
        let bph = view blockPayloadHash blockHeader
            enc = runPutS $ encodeBlockPayloadHash bph

        assert (b_payload == enc) $
          "Payload Hash mismatch at block " ++ show a_bh ++ " / txid " ++ show a_etxid

        n2' <- nTableEntries sql "BlockHistory2"
        assert (n == n2') "BlockHistory2 has the same number of rows as BlockHistory"
      _ -> error "unexpected result shape"
  pure ()

assert :: Bool -> String -> IO ()
assert = flip assertBool

nTableEntries :: SQLiteEnv -> Utf8 -> IO Int64
nTableEntries sql tname = throwOnDbError $ qry_ sql ("SELECT count(*) from " <> tname) [RInt] >>= \case
      [[SInt n]] -> pure n
      _ -> error "unexpected row shape"
