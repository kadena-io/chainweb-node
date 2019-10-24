{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CheckpointerDBChecksum where

import Control.Monad.Reader
import Control.Exception.Safe (tryAny)

import Data.ByteString (ByteString)
import Data.ByteString.Builder
import qualified Data.HashSet as HashSet
import Data.Int
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Serialize
import Data.String.Conv
import Data.Text (Text)

import Database.SQLite3.Direct as SQ3

import GHC.Generics
import GHC.IO.Handle

import System.IO

-- pact imports
import Pact.Types.SQLite

-- chainweb imports
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils hiding (callDb)
import Chainweb.Pact.Service.Types

main :: IO ()
main = putStrLn "Yay, compiles!"

type TableName = ByteString
type TableContents = ByteString

-- this will produce both the raw bytes for all of the tables concatenated
-- together, and the raw bytes of each individual table (this is over a single chain)
work :: Args -> BlockHeight -> BlockHeight -> IO (Builder, Map TableName TableContents)
work args low high = withSQLiteConnection (aFilePath args) chainwebPragmas False (runReaderT go)
  where
    go = do
        let systemtables = foldr ((.).(:)) id
              [ "BlockHistory"
              , "VersionedTableCreation"
              , "VersionedTableMutation"
              , "TransactionIndex"
              , "[SYS:KeySets]"
              , "[SYS:Modules]"
              , "[SYS:Namespaces]"
              , "[SYS:Pacts]"
              ]
        names <- systemtables <$> getUserTables (aStartBlockHeight args) (aEndBlockHeight args)
        callDb "getting rows" $ \db -> liftIO $ foldM (collect db) (mempty, mempty) names

    collect db (entiredb,  m) name = do
        rows <- getTblContents db name
        let !bytes = encode rows
        return (mappend entiredb (byteString bytes), M.insert name bytes m)

    getTblContents db = \case
        "TransactionIndex" -> qry db tistmt (SInt . fromIntegral <$> [low, high]) [RBlob, RInt]
        "BlockHistory" -> qry db bhstmt (SInt . fromIntegral <$> [low, high]) [RInt, RBlob, RInt]
        "VersionedTableCreation" -> qry db vtcstmt (SInt . fromIntegral <$> [low, high]) [RText, RInt]
        "VersionedTableMutation" -> qry db vtmstmt (SInt . fromIntegral <$> [low, high]) [RText, RInt]
        t -> do
            lowtxid <- getActualStartingTxId low db
            hightxid <- getActualEndingTxId high db
            qry db (usertablestmt t) (SInt <$> [lowtxid, hightxid]) [RText, RInt, RBlob]

    getActualStartingTxId :: BlockHeight -> Database -> IO Int64
    getActualStartingTxId bh db =
        if bh == 0 then return 0 else do
            r <- qry db stmt [SInt $ pred $ fromIntegral bh] [RInt]
            case r of
                [[SInt txid]] -> return $ succ txid
                _ -> error "Cannot find starting txid."
      where
        stmt = "SELECT endingtxid FROM BlockHistory WHERE blockheight  = ?;"

    getActualEndingTxId :: BlockHeight -> Database -> IO Int64
    getActualEndingTxId bh db =
        if bh == 0 then return 0 else do
            r <- qry db stmt [SInt $ fromIntegral bh] [RInt]
            case r of
                [[SInt txid]] -> return txid
                _ -> error "Cannot find ending txid."
      where
        stmt = "SELECT endingtxid FROM BlockHistory WHERE blockheight = ?;"

    tistmt = "SELECT * FROM TransactionIndex ORDER BY blockheight,txhash DESC WHERE blockheight >= ? AND blockheight <= ?;"
    bhstmt = "SELECT * FROM BlockHistory ORDER BY blockheight, endingtxid, hash DESC WHERE blockheight >= ? AND blockheight <= ?;"
    vtcstmt = "SELECT * FROM VersionedTableCreation ORDER BY createBlockheight, tablename DESC WHERE createBlockheight >= ? AND createBlockheight <= ?;"
    vtmstmt = "SELECt * FROM VersionedTableMutation ORDER BY blockheight, tablename DESC WHERE blockheight >= ? AND blockheight <= ?;"
    usertablestmt tbl =
      "SELECT * FROM [" <> Utf8 tbl <> "] ORDER BY txid DESC ORDER BY rowkey ASC WHERE txid > ? AND txid <= ?;"

builderToFile :: FilePath -> Builder -> IO ()
builderToFile fp builder =
    withFile fp WriteMode $ \handle -> do
        hSetBinaryMode handle True
        hSetBuffering handle (BlockBuffering (Just blockSize))
        hPutBuilder handle builder
  where
    blockSize = 80 -- This is a made up number. Change if necessary

-- this function is not necessary
{-
bytestringToFile :: FilePath -> ByteString -> IO ()
bytestringToFile fp bytestring = B.writeFile fp bytestring
-}

callDb :: Text -> (Database -> ReaderT SQLiteEnv IO a) -> ReaderT SQLiteEnv IO a
callDb callerName action = do
    c <- asks _sConn
    tryAny (action c) >>= \case
      Left err -> internalError $ "callDb (" <> callerName <> "): " <> toS (show err)
      Right r -> return r

getUserTables :: BlockHeight -> BlockHeight -> ReaderT SQLiteEnv IO [ByteString]
getUserTables low high = callDb "getUserTables" $ \db -> liftIO $ do
    usertables <- fmap toByteString . concat <$> qry db stmt (SInt . fromIntegral <$> [low, high]) [RText]
    check db usertables
    return usertables
  where
    toByteString (SText (Utf8 bytestring)) = bytestring
    toByteString _ = error "impossible case"
    stmt = "SELECT DISTINCT tablename FROM VersionedTableCreation ORDER BY tablename DESC WHERE createBlockheight >= ? AND createBlockheight <= ?;"
    check db tbls = do
        r <- HashSet.fromList . fmap toByteString . concat <$> qry_ db alltables [RText]
        when (HashSet.null r) $ internalError errMsg
        let res = getFirst $ foldMap (\tbl -> First $ if HashSet.member tbl r then Nothing else Just tbl) tbls
        maybe (return ()) (internalError . tableErrMsg . toS) res
      where
        errMsg = "Somehow there are no tables in this connection. This should be an impossible case."
        alltables = "SELECT name FROM sqlite_master WHERE type='table';"
        tableErrMsg tbl = "This is table " <> tbl <> " is listed in VersionedTableCreation but is not actually in the database."

data Args = Args
   {  aFilePath   :: FilePath
   , aStartBlockHeight :: BlockHeight
   , aEndBlockHeight :: BlockHeight
   }

deriving instance Generic SType
deriving instance Serialize SType
deriving instance Generic Utf8
deriving instance Serialize Utf8
