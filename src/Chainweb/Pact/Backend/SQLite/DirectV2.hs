{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chainweb.Pact.Backend.SQLite.DirectV2
( SQLiteFlag(..)
, open_v2
, close_v2
, init_sha3
) where

import qualified Data.ByteString as BS

import Foreign
import Foreign.C.String

import Database.SQLite3.Direct
import Database.SQLite3.Bindings.Types

-- chainweb
import Chainweb.Pact.Backend.SQLite.V2

import Foreign.C.Types (CInt)


newtype SQLiteFlag = SQLiteFlag { getFlag :: CInt }
  deriving newtype (Eq, Ord, Bits, Num)

open_v2 :: Utf8 -> SQLiteFlag -> Maybe Utf8 -> IO (Either (Error, Utf8) Database)
open_v2 (Utf8 path) (SQLiteFlag flag) mzvfs =
    BS.useAsCString path $ \path' -> do
      useAsMaybeCString mzvfs $ \zvfs' ->
        alloca $ \database -> do
            rc <- c_sqlite3_open_v2 path' database flag zvfs'
            db <- Database <$> peek database
                -- sqlite_open_v2 returns a sqlite3 even on failure.
                -- That's where we get a more descriptive error message.
            case toResult () rc of
              Left err -> do
                msg <- errmsg db -- This returns "out of memory" if db is null.
                _ <- close db -- This is harmless if db is null.
                return $ Left (err, msg)
              Right () ->
                if db == Database nullPtr
                then error "sqlite3_open_v2 unexpectedly returned NULL"
                else do
                  init_sha3 db >>= \case
                    Left err -> do
                      msg <- errmsg db -- This returns "out of memory" if db is null.
                      _ <- close db -- This is harmless if db is null.
                      return $ Left (err, msg)
                    Right () ->
                        return $ Right db
  where
    useAsMaybeCString :: Maybe Utf8 -> (CString -> IO a) -> IO a
    useAsMaybeCString (Just (Utf8 zvfs)) f = BS.useAsCString zvfs f
    useAsMaybeCString _ f = f nullPtr

close_v2 :: Database -> IO (Either Error ())
close_v2 (Database db) =
  toResult () <$> c_sqlite3_close_v2 db

init_sha3 :: Database -> IO (Either Error ())
init_sha3 (Database db) =
    toResult () <$> c_sqlite3_shathree_create_functions db

toResult :: a -> CError -> Either Error a
toResult a (CError 0) = Right a
toResult _ code = Left $ decodeError code
