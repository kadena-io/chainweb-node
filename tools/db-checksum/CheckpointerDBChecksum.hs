{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CheckpointerDBChecksum where

import Configuration.Utils hiding (Error, Lens', action, encode)

import Control.Exception.Safe (tryAny)
import Control.Monad.Reader

import Crypto.Hash

import Data.ByteArray (convert)
import qualified Data.ByteString as B (ByteString, writeFile)
import Data.ByteString.Builder
import qualified Data.HashSet as HashSet
import Data.Int
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Serialize
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Database.SQLite3.Direct as SQ3

import GHC.Generics

import System.Directory

import Text.Printf

-- pact imports
import Pact.Types.SQLite

-- chainweb imports
import Chainweb.BlockHeight
import Chainweb.Pact.Backend.Types
import Chainweb.Pact.Backend.Utils hiding (callDb)
import Chainweb.Pact.Service.Types
import Chainweb.Utils hiding (check)

main :: IO ()
main = runWithConfiguration mainInfo $ \args -> do
    (entiredb, tables) <- work args
    case _getAllTables args of
        True -> do
            putStrLn "----------Computing \"entire\" db----------"
            let !checksum = convert @(Digest SHA1) @B.ByteString . hashlazy $ toLazyByteString entiredb
            exists <- doesFileExist $ _entireDBOutputFile args
            -- Just rewrite the file. Let's not do anything complicated here.
            when exists $ removeFile $ _entireDBOutputFile args
            B.writeFile (_entireDBOutputFile args) checksum
        False -> do
            putStrLn "----------Computing tables----------"
            let dir = _tablesOutputLocation args
            createDirectoryIfMissing True dir
            files <- listDirectory dir
            mapM_ (\file -> removeFile (dir <> "/" <> file)) files
            void $ M.traverseWithKey (go (_tablesOutputLocation args)) tables
    putStrLn "----------All done----------"
  where
    go :: String -> B.ByteString -> B.ByteString -> IO ()
    go dir tablename tablebytes = do
        let !checksum = convert @(Digest SHA1) @B.ByteString . hash $ tablebytes
        B.writeFile (dir <> "/" <> T.unpack (T.decodeUtf8 tablename)) checksum

type TableName = B.ByteString
type TableContents = B.ByteString

-- this will produce both the raw bytes for all of the tables concatenated
-- together, and the raw bytes of each individual table (this is over a single chain)
work :: Args -> IO (Builder, Map TableName TableContents)
work args = withSQLiteConnection (_sqliteFile args) chainwebPragmas (runReaderT go)
  where
    low = _startBlockHeight args
    high = case _endBlockHeight args of
      Nothing -> BlockHeight <$> getMaxHeight
      Just h@(BlockHeight bheight) -> do
        maxheight <- getMaxHeight
        when (bheight > maxheight) $
          error (printf "Requested height %d is greater than the greatest recorded height %d in the BlockHistory table" bheight maxheight)
        return h

    go = do
        h <- high
        names <- todl systemtables <$> getUserTables low h
        foldM (collect h) (mempty, mempty) names

    todl = foldr (\a b ->  (a:) . b) id

    getMaxHeight = (callDb dbcall $ \db -> qry_ db maxheightstmt [RInt]) >>= \case
        [[SInt maxheight]] -> return $ fromIntegral maxheight
        err ->
          error (printf "Encountered this result %s while trying to get highest blockheight" (show err))
      where
        maxheightstmt = "SELECT MAX(blockheight) FROM BlockHistory;"
        dbcall = "Getting max height from BlockHistory"

    systemtables =
      [ "BlockHistory"
      , "VersionedTableCreation"
      , "VersionedTableMutation"
      , "TransactionIndex"
      , "[SYS:KeySets]"
      , "[SYS:Modules]"
      , "[SYS:Namespaces]"
      , "[SYS:Pacts]"
      ]

    collect h (entiredb,  m) name = do
        rows <- getTblContents h name
        let !bytes = encode rows
        return (mappend entiredb (byteString bytes), M.insert name bytes m)

    getTblContents h = \case
        "TransactionIndex" -> do
          callDb "TransactionIndex" $ \db ->
            qry db tistmt (SInt . fromIntegral <$> [low, h]) [RBlob, RInt]
        "BlockHistory" -> do
          callDb "BlockHistory" $ \db ->
            qry db bhstmt (SInt . fromIntegral <$> [low, h]) [RInt, RBlob, RInt]
        "VersionedTableCreation" ->
          callDb "VersionedTableCreation" $ \db ->
            qry db vtcstmt (SInt . fromIntegral <$> [low, h]) [RText, RInt]
        "VersionedTableMutation" ->
          callDb "VersionedTableMutation" $ \db ->
            qry db vtmstmt (SInt . fromIntegral <$> [low, h]) [RText, RInt]
        t -> do
            lowtxid <-
              callDb "starting txid" $ \db ->
                getActualStartingTxId low db
            hightxid <-
              callDb "last txid" $ \db ->
                getActualEndingTxId h db
            callDb ("on table " <> T.decodeUtf8 t) $ \db ->
              qry db (usertablestmt t) (SInt <$> [lowtxid, hightxid]) [RText, RInt, RBlob]

    getActualStartingTxId :: BlockHeight -> Database -> IO Int64
    getActualStartingTxId bh db =
        if bh == 0 then return 0 else do
            r <- qry db stmt [SInt $ max 0 $ pred $ fromIntegral bh] [RInt]
            case r of
                [[SInt txid]] -> return $ succ txid
                _ -> internalError "Cannot find starting txid."
      where
        stmt = "SELECT endingtxid FROM BlockHistory WHERE blockheight  = ?;"

    getActualEndingTxId :: BlockHeight -> Database -> IO Int64
    getActualEndingTxId bh db =
        if bh == 0 then return 0 else do
            r <- qry db stmt [SInt $ fromIntegral bh] [RInt]
            case r of
                [[SInt txid]] -> return txid
                _ -> internalError "Cannot find ending txid."
      where
        stmt = "SELECT endingtxid FROM BlockHistory \
               \WHERE blockheight = ?;"

    tistmt = "SELECT * FROM TransactionIndex\
             \ WHERE blockheight >= ? \
             \AND blockheight <= ? \
             \ORDER BY blockheight,txhash DESC;"
    bhstmt = "SELECT * FROM BlockHistory \
             \WHERE blockheight >= ? AND blockheight <= ? \
             \ORDER BY blockheight, endingtxid, hash DESC;"
    vtcstmt = "SELECT * FROM VersionedTableCreation \
              \WHERE createBlockheight >= ? AND createBlockheight <= ? \
              \ORDER BY createBlockheight, tablename DESC;"
    vtmstmt = "SELECT * FROM VersionedTableMutation\
              \ WHERE blockheight >= ? AND blockheight <= ? \
              \ORDER BY blockheight, tablename DESC;"
    usertablestmt = \case
      "[SYS:KeySets]" -> "SELECT * FROM [SYS:KeySets] \
                         \WHERE txid > ? AND txid <= ? \
                         \ORDER BY txid DESC, rowkey ASC, rowdata ASC;"
      "[SYS:Modules]" -> "SELECT * FROM [SYS:Modules] \
                         \WHERE txid > ? AND txid <= ? \
                         \ORDER BY txid DESC, rowkey ASC, rowdata ASC;"
      "[SYS:Namespaces]" -> "SELECT * FROM [SYS:Namespaces] \
                            \WHERE txid > ? AND txid <= ? \
                            \ORDER BY txid DESC, rowkey ASC, rowdata ASC;"
      "[SYS:Pacts]" -> "SELECT * FROM [SYS:Pacts] \
                       \WHERE txid > ? AND txid <= ? \
                       \ORDER BY txid DESC, rowkey ASC, rowdata ASC;"
      tbl -> "SELECT * FROM ["
        <> Utf8 tbl
        <> "] WHERE txid > ? AND txid <= ? ORDER BY txid DESC, rowkey ASC, rowdata ASC;"

callDb :: T.Text -> (Database -> IO a) -> ReaderT SQLiteEnv IO a
callDb callerName action = do
    c <- asks _sConn
    tryAny (liftIO $ action c) >>= \case
      Left err -> internalError $ "callDb (" <> callerName <> "): " <> sshow err
      Right r -> return r

getUserTables :: BlockHeight -> BlockHeight -> ReaderT SQLiteEnv IO [B.ByteString]
getUserTables low high = callDb "getUserTables" $ \db -> do
    usertables <-
      (traverse toByteString . concat) <$> qry db stmt (SInt . fromIntegral <$> [low, high]) [RText]
      >>= eInternalError

    check db usertables
    return usertables
  where
    eInternalError = either internalError return
    toByteString (SText (Utf8 bytestring)) = Right bytestring
    toByteString _ = Left ("impossible case" :: T.Text)
    stmt = "SELECT DISTINCT tablename FROM VersionedTableCreation\
           \ WHERE createBlockheight >= ? AND createBlockheight <= ? \
           \ORDER BY tablename DESC;"
    check :: Database -> [B.ByteString] -> IO ()
    check db tbls = do
        r <-
          fmap HashSet.fromList . traverse toByteString . concat <$> qry_ db alltables [RText]
          >>= eInternalError

        when (HashSet.null r) $ internalError errMsg
        let res = getFirst $ foldMap (\tbl -> First $ if HashSet.member tbl r then Nothing else Just tbl) tbls
        maybe (return ()) (internalError . tableErrMsg . T.decodeUtf8) res
      where
        errMsg = "Somehow there are no tables in this connection. This should be an impossible case."
        alltables = "SELECT name FROM sqlite_master WHERE type='table';"
        tableErrMsg tbl = "This table " <> tbl <> " is listed in VersionedTableCreation but is not actually in the database."

data Args = Args
   {  _sqliteFile :: FilePath
   , _startBlockHeight :: BlockHeight
   , _endBlockHeight :: Maybe BlockHeight
   , _entireDBOutputFile :: FilePath
   , _tablesOutputLocation :: String
   , _getAllTables :: Bool
   } deriving (Show, Generic)

sqliteFile :: Functor f => (FilePath -> f FilePath) -> Args -> f Args
sqliteFile f s = (\u -> s { _sqliteFile = u }) <$> f (_sqliteFile s)

startBlockHeight :: Functor f => (BlockHeight -> f BlockHeight) -> Args -> f Args
startBlockHeight f s = (\u -> s { _startBlockHeight = u }) <$> f (_startBlockHeight s)

endBlockHeight :: Functor f => (Maybe BlockHeight -> f (Maybe BlockHeight)) -> Args -> f Args
endBlockHeight f s = (\u -> s { _endBlockHeight = u }) <$> f (_endBlockHeight s)

entireDBOutputFile :: Functor f => (FilePath -> f FilePath) -> Args -> f Args
entireDBOutputFile f s = (\u -> s { _entireDBOutputFile = u }) <$> f (_entireDBOutputFile s)

tablesOutputLocation :: Functor f => (String -> f String) -> Args -> f Args
tablesOutputLocation f s = (\u -> s { _tablesOutputLocation = u }) <$> f (_tablesOutputLocation s)

getAllTables :: Functor f => (Bool -> f Bool) -> Args -> f Args
getAllTables f s = (\u -> s { _getAllTables = u }) <$> f (_getAllTables s)

instance ToJSON Args where
  toJSON o = object
      [ "sqliteFile" .= _sqliteFile o
      , "startBlockHeight" .= _startBlockHeight o
      , "endBlockHeight" .= _endBlockHeight o
      , "entireDBOutputFile" .= _entireDBOutputFile o
      , "tablesOutputLocation" .= _tablesOutputLocation o
      , "getAllTables" .= _getAllTables o
      ]

instance FromJSON (Args -> Args) where
    parseJSON = withObject "Args" $ \o -> id
      <$< sqliteFile ..: "sqliteFile" % o
      <*< startBlockHeight ..: "startBlockHeight" % o
      <*< endBlockHeight ..: "endBlockHeight" % o
      <*< entireDBOutputFile ..: "entireDBOutputFile" % o
      <*< tablesOutputLocation ..: "tablesOutputLocation" % o
      <*< getAllTables ..: "getAllTables" % o

argsParser :: MParser Args
argsParser = id
  <$< sqliteFile .:: textOption
      % long "sqlite-file"
      <> metavar "FILE"
      <> help "Pact sqlite file"
  <*< startBlockHeight .:: option auto
    % long "initial-blockheight"
    <> metavar "BLOCKHEIGHT"
    <> help "Initial blockheight"
  <*< endBlockHeight .:: option auto
    % long "final-blockheight"
    <> metavar "BLOCKHEIGHT"
    <> help "Final blockheight"
  <*< entireDBOutputFile .:: option auto
    % long "output-db-file"
    <> metavar "FILE"
    <> help "Location to dump the sha1 checksum of some portion (limited by some range of blocks) of the database"
  <*< tablesOutputLocation .:: option auto
    % long "tables-output-dir"
    <> metavar "DIRECTORY"
    <> help "Directory where sha1 checksums of each individual table will be stored."
  <*< getAllTables .:: option auto
    % long "get-all-tables"
    <> metavar "BOOL"
    <> help "Flag to decide whether to get hash of all tables or individual hashes of each table."


defaultArgs :: Args
defaultArgs =
    Args
        { _sqliteFile = "no-known-file"
          , _startBlockHeight = 0
          , _endBlockHeight = Nothing
          , _entireDBOutputFile = "dboutput.hash"
          , _tablesOutputLocation = "dboutputhashes"
          , _getAllTables = True
        }

mainInfo :: ProgramInfo Args
mainInfo =
    programInfo
    "CheckpointerDBChecksum"
    argsParser
    defaultArgs

deriving instance Generic SType
deriving instance Serialize SType
deriving instance Generic Utf8
deriving instance Serialize Utf8

deriving instance Read BlockHeight
