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
-- together, and the raw bytes of each individual table
work :: Args -> FilePath -> IO (Builder, Map TableName TableContents)
work _args sqlitefile = withSQLiteConnection sqlitefile chainwebPragmas False (runReaderT go)
  where
    go = do
        names <- getTableNames
        callDb "getting rows" $ \db -> liftIO $ foldM (collect db) (mempty, mempty) names

    collect db (entiredb,  m) name = do
        rows <- getTblContents db name
        let !bytes = encode rows
        return ((mappend entiredb (byteString bytes)), (M.insert name bytes m))

    getTblContents db = \case
        "TransactionIndex" -> qry_ db tistmt [RBlob, RInt]
        "BlockHistory" -> qry_ db bhstmt [RInt, RBlob, RInt]
        "VersionedTableCreation" -> qry_ db vtcstmt [RText, RInt]
        "VersionedTableMutation" -> qry_ db vtmstmt [RText, RInt]
        t -> qry_ db (usertablestmt t) [RText, RInt, RBlob]

    tistmt = "SELECT * FROM TransactionIndex ORDER BY blockheight DESC;"
    bhstmt = "SELECT * FROM BlockHistory ORDER BY blockheight, endingtxid, hash DESC;"
    vtcstmt = "SELECT * FROM VersionedTableCreation ORDER BY createBlockheight DESC;"
    vtmstmt = "SELECt * FROM VersionedTableMutation ORDER BY blockheight DESC;"
    usertablestmt tbl =
      "SELECT * FROM [" <> Utf8 tbl <> "] ORDER BY txid DESC ORDER BY rowkey ASC;"

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
