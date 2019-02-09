{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Chainweb.Test.PactService
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact execution in Chainweb

module Chainweb.Test.Pact.Utils where

import Control.Monad.Zip

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Default
import Data.Maybe
import qualified Data.Text as T
import Data.Word

import System.IO.Extra

import qualified Pact.ApiReq as P
import qualified Pact.Types.Command as P
import qualified Pact.Types.Crypto as P
import qualified Pact.Types.RPC as P

import Chainweb.Pact.Types

testMemPoolAccess :: TransactionCriteria -> IO [Transaction]
testMemPoolAccess _criteria = do
    moduleStr <- readFile' $ testPactFilesDir ++ "test1.pact"
    let cmdStrs =
          [ moduleStr
          , "(create-table test1.accounts)"
          , "(test1.create-global-accounts)"
          , "(test1.transfer \"Acct1\" \"Acct2\" 1.00)" ]
    mkPactTestTransactions cmdStrs

mkPactTestTransactions :: [String] -> IO [Transaction]
mkPactTestTransactions cmdStrs = do
    let theData = object ["test-admin-keyset" .= fmap P._kpPublic testKeyPairs]
    let intSeq = [0, 1 ..] :: [Word64]
    -- using 1 as the nonce here so the hashes match for the same commands (for testing only)
    return $ zipWith (mkPactTransaction testKeyPairs theData "1" )
             intSeq cmdStrs

mkPactTransaction
  :: [P.KeyPair]
  -> Value
  -> T.Text
  -> Word64
  -> String
  -> Transaction
mkPactTransaction keyPair theData nonce txId theCode =
    let pubMeta = def :: P.PublicMeta
        cmd = P.mkCommand
              (map (\P.KeyPair {..} -> (P.ED25519, _kpSecret, _kpPublic)) keyPair)
              pubMeta
              nonce
              (P.Exec (P.ExecMsg (T.pack theCode) theData))
    in Transaction {_tTxId = txId, _tCmd = cmd}

testKeyPairs :: [P.KeyPair]
testKeyPairs =
    let mPair = mzip (P.importPrivate testPrivateBs) (P.importPublic testPublicBs)
        mKeyPair = fmap
                   (\(sec, pub) -> P.KeyPair {_kpSecret = sec, _kpPublic = pub})
                   mPair
    in maybeToList mKeyPair

testPactFilesDir :: String
testPactFilesDir = "test/config/"

testPrivateBs :: ByteString
testPrivateBs = "53108fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d2"

testPublicBs :: ByteString
testPublicBs = "201a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c0dbc"
