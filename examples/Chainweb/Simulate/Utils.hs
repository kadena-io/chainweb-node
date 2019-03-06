{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Chainweb.Simulate.Utils where

import Control.Monad.Zip

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Default (def)
import Data.Maybe (catMaybes, maybeToList)
import Data.Text (Text)

import GHC.Generics

import NeatInterpolation

import Pact.ApiReq (KeyPair(..))
import Pact.Types.Command (mkCommand, Command (..), PublicMeta)
import Pact.Types.Crypto (PPKScheme(..), importPublic, importPrivate)
import Pact.Types.RPC (PactRPC(..), ExecMsg(..))

testAdminPrivates :: ByteString
testAdminPrivates =
  "53108fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d2"

testAdminPublics :: ByteString
testAdminPublics =
  "201a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c0dbc"

testPrivates :: [ByteString]
testPrivates =
  [ "53108fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d3"
  , "53108fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
  ]

testPublics :: [ByteString]
testPublics =
  [ "201a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c1dbc"
  , "201a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
  ]

testAdminKeyPairs :: [KeyPair]
testAdminKeyPairs =
  let mPair =
        mzip (importPrivate testAdminPrivates) (importPublic testAdminPublics)
      mKeyPair =
        fmap (\(sec, pub) -> KeyPair {_kpSecret = sec, _kpPublic = pub}) mPair
   in maybeToList mKeyPair

testKeyPairs :: [KeyPair]
testKeyPairs =
  catMaybes $
  zipWith
    (\sec pub -> KeyPair <$> (importPrivate sec) <*> (importPublic pub))
    testPublics
    testPrivates

createTable :: Nonce -> TableName -> Command ByteString
createTable (getNonce -> nonce) (getTableName -> tableName) = command
  where
    command = mkCommand [] (def :: PublicMeta) nonce (Exec (ExecMsg theCode theData))
    theCode = [text|(create-table $tableName)|]
    theData = Null

newtype Nonce = Nonce {getNonce :: Text} deriving (Eq, Show, Generic)

newtype TableName = TableName {getTableName :: Text} deriving (Eq, Show, Generic)
