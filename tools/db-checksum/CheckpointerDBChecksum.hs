{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CheckpointerDBChecksum where

import Control.Monad.Reader
import Control.Exception.Safe (tryAny)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder
import Data.Int
import Data.Map (Map)
import qualified Data.Map as M
import Data.Serialize
import Data.Text (pack, Text)

import Database.SQLite3.Direct as SQ3

import GHC.Generics
import GHC.IO.Handle

import System.IO

-- pact imports
import Pact.Types.SQLite

-- chainweb imports
import Chainweb.BlockHash
import Chainweb.BlockHeader
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils hiding (callDb)
import Chainweb.Pact.Service.Types
import Chainweb.Version

main :: IO ()
main = putStrLn "Yay, compiles!"


type TableName = ByteString
type TableContents = ByteString

-- this will produce both the raw bytes for all of the tables concatenated
-- together, and the raw bytes of each individual table (this is over a single chain)
work :: Args -> FilePath -> BlockHeight -> IO (Builder, Map TableName TableContents)
work _args sqlitefile limit = withSQLiteConnection sqlitefile chainwebPragmas False (runReaderT go)
  where
    go = do
        names <- getTableNames
        callDb "getting rows" $ \db -> liftIO $ foldM (collect db) (mempty, mempty) names

    collect db (entiredb,  m) name = do
        rows <- getTblContents db name
        let !bytes = encode rows
        return ((mappend entiredb (byteString bytes)), (M.insert name bytes m))

    getTblContents db = \case
        "TransactionIndex" -> qry db tistmt [SInt $ fromIntegral limit] [RBlob, RInt]
        "BlockHistory" -> qry db bhstmt [SInt $ fromIntegral limit] [RInt, RBlob, RInt]
        "VersionedTableCreation" -> qry db vtcstmt [SInt $ fromIntegral limit] [RText, RInt]
        "VersionedTableMutation" -> qry db vtmstmt [SInt $ fromIntegral limit] [RText, RInt]
        t -> do
          txid <- getEndingTxId limit db
          qry db (usertablestmt t) [SInt txid] [RText, RInt, RBlob]

    getEndingTxId :: BlockHeight -> Database -> IO Int64
    getEndingTxId bh db = do
        r <- qry db stmt [SInt $ fromIntegral bh] [RInt]
        case r of
          [[SInt txid]] -> return txid
          _ -> error "Cannot find ending txid."
      where
        stmt = "SELECT endingtxid FROM BlockHistory WHERE blockheight = ?;"

    tistmt = "SELECT * FROM TransactionIndex ORDER BY blockheight DESC WHERE blockheight <= ?;"
    bhstmt = "SELECT * FROM BlockHistory ORDER BY blockheight, endingtxid, hash DESC WHERE blockheight <= ?;"
    vtcstmt = "SELECT * FROM VersionedTableCreation ORDER BY createBlockheight DESC WHERE createBlockheight <= ?;"
    vtmstmt = "SELECt * FROM VersionedTableMutation ORDER BY blockheight DESC WHERE blockheight <= ?;"
    usertablestmt tbl =
      "SELECT * FROM [" <> Utf8 tbl <> "] ORDER BY txid DESC ORDER BY rowkey ASC WHERE txid <= ?;"

builderToFile :: FilePath -> Builder -> IO ()
builderToFile fp builder =
    withFile fp WriteMode $ \handle -> do
        hSetBinaryMode handle True
        hSetBuffering handle (BlockBuffering (Just blockSize))
        hPutBuilder handle builder
  where
    blockSize = 80 -- This is a made up number. Change if necessary

bytestringToFile :: FilePath -> ByteString -> IO ()
bytestringToFile fp bytestring = B.writeFile fp bytestring

callDb :: Text -> (Database -> ReaderT SQLiteEnv IO a) -> ReaderT SQLiteEnv IO a
callDb callerName action = do
    c <- asks _sConn
    tryAny (action c) >>= \case
      Left err -> internalError $ "callDb (" <> callerName <> "): " <> (pack $ show err)
      Right r -> return r

getTableNames :: ReaderT SQLiteEnv IO [ByteString]
getTableNames = callDb "getTableNames" $ \db -> do
    r <- concat <$> (liftIO $ qry_ db stmt [RText])
    case r of
      [] -> internalError errMsg
      _ -> return $ toByteString <$> r
  where
    stmt = "SELECT name FROM sqlite_master WHERE type='table';"
    toByteString (SText (Utf8 bytestring)) = bytestring
    toByteString _ = error "impossible case"
    errMsg = "Somehow there are no tables in this connection. This should be an impossible case."


data Args = Args
   {  aFilePath    :: FilePath
   , aBlockHeight :: BlockHeight
   , aBlockHash   :: BlockHash
   , aVersion     :: ChainwebVersion
   }

deriving instance Generic SType
deriving instance Serialize SType
deriving instance Generic Utf8
deriving instance Serialize Utf8
