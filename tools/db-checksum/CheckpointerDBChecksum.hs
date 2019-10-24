{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CheckpointerDBChecksum where

import Configuration.Utils hiding (action, encode, Error, Lens', (<.>))

import Control.Monad.Reader
import Control.Exception.Safe (tryAny)

import qualified Data.Binary as Binary
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

import Data.Digest.Pure.SHA

import GHC.Generics

-- pact imports
import Pact.Types.SQLite

-- chainweb imports
import Chainweb.BlockHeader
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
          let !checksum = sha1 $ toLazyByteString entiredb
          Binary.encodeFile (_entireDBOutputFile args) checksum
        False -> do
          putStrLn "----------Computing tables----------"
          void $ M.traverseWithKey (go (_tablesOutputLocation args)) tables
    putStrLn "----------All done----------"
  where
    go :: String -> ByteString -> ByteString -> IO ()
    go dir tablename tablebytes = do
      let !checksum = sha1 $ toS tablebytes
      Binary.encodeFile (dir <> "/" <> toS tablename) checksum


type TableName = ByteString
type TableContents = ByteString

-- this will produce both the raw bytes for all of the tables concatenated
-- together, and the raw bytes of each individual table (this is over a single chain)
work :: Args -> IO (Builder, Map TableName TableContents)
work args = withSQLiteConnection (_sqliteFile args) chainwebPragmas False (runReaderT go)
  where
    low = _startBlockHeight args
    high = _endBlockHeight args
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

        names <- systemtables <$> getUserTables low high
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

-- builderToFile :: FilePath -> Builder -> IO ()
-- builderToFile fp builder =
--     withFile fp WriteMode $ \handle -> do
--         hSetBinaryMode handle True
--         hSetBuffering handle (BlockBuffering (Just blockSize))
--         hPutBuilder handle builder
--   where
--     blockSize = 80 -- This is a made up number. Change if necessary

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
   {  _sqliteFile   :: FilePath
   , _startBlockHeight :: BlockHeight
   , _endBlockHeight :: BlockHeight
   , _entireDBOutputFile :: FilePath
   , _tablesOutputLocation :: String
   , _getAllTables :: Bool
   } deriving (Show, Generic)

sqliteFile :: Functor f => (FilePath -> f FilePath) -> Args -> f Args
sqliteFile f s = (\u -> s { _sqliteFile = u }) <$> f (_sqliteFile s)

startBlockHeight :: Functor f => (BlockHeight -> f BlockHeight) -> Args -> f Args
startBlockHeight f s = (\u -> s { _startBlockHeight = u }) <$> f (_startBlockHeight s)

endBlockHeight :: Functor f => (BlockHeight -> f BlockHeight) -> Args -> f Args
endBlockHeight f s = (\u -> s { _endBlockHeight = u }) <$> f (_endBlockHeight s)

entireDBOutputFile :: Functor f => (FilePath -> f FilePath) -> Args -> f Args
entireDBOutputFile f s = (\u -> s { _entireDBOutputFile = u }) <$> f (_entireDBOutputFile s)

tablesOutputLocation :: Functor f => (String -> f String) -> Args -> f Args
tablesOutputLocation f s = (\u -> s { _tablesOutputLocation = u }) <$> f (_tablesOutputLocation s)

getAllTables :: Functor f => (Bool -> f Bool) -> Args -> f Args
getAllTables f s = (\u -> s { _getAllTables = u }) <$> f (_getAllTables s)

instance ToJSON Args where
  toJSON o = object
      [ "sqliteFile"          .= _sqliteFile o
      , "startBlockHeight"    .= _startBlockHeight o
      , "endBlockHeight"      .= _endBlockHeight o
      , "entireDBOutputFile"  .= _entireDBOutputFile o
      , "tablesOutputLocation" .= _tablesOutputLocation o
      , "_tablesOutputLocation" .= _tablesOutputLocation o
      , "_getAllTables" .= _getAllTables o
      ]

instance FromJSON (Args -> Args) where
    parseJSON = withObject "Args" $ \o -> id
      <$< sqliteFile          ..: "sqliteFile"          % o
      <*< startBlockHeight    ..: "startBlockHeight"    % o
      <*< endBlockHeight      ..: "endBlockHeight"      % o
      <*< entireDBOutputFile  ..: "entireDBOutputFile"  % o
      <*< tablesOutputLocation ..: "tablesOutputLocation" % o
      <*< getAllTables ..: "getAllTables" % o

argsParser :: MParser Args
argsParser = id
  <$< sqliteFile .:: textOption
      % long "The sqlite file"
      <> metavar "FILE"
      <> help "Pact sqlite file"
  <*< startBlockHeight .:: option auto
    % long "Starting blockheight"
    <> metavar "BlockHeight"
  <*< endBlockHeight .:: option auto
    % long "LastBlockheight"
    <> metavar "BlockHeight"
    <> help "Last blockheight"
  <*< entireDBOutputFile .:: option auto
    % long "outputDBFile"
    <> metavar "FILE"
    <> help "Location to dump the sha1 checksum of some portion (limited by some range of blocks) of the database"
  <*< tablesOutputLocation .:: option auto
    % long "tablesOutputLocation"
    <> metavar "DIRECTORY"
    <> help "Directory where sha1 checksums of each individual table will be stored."
  <*< getAllTables .:: option auto
    % long "getAllTables"
    <> metavar "BOOL"
    <> help "Flag to decide whether to get hash of all tables or individual hashes of each table."


defaultArgs :: Args
defaultArgs =
    Args
        {
            _sqliteFile = error "There seems to be no good default for this field."
          , _startBlockHeight = 0
          , _endBlockHeight = error "There seems to be no good default for this field."
          , _entireDBOutputFile = error "There seems to be no good default for this field."
          , _tablesOutputLocation = error "There seems to be no good default for this field."
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
