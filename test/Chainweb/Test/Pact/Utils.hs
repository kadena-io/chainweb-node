{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: Chainweb.Test.PactInProcApi
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Emily Pillmore <emily@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact execution via (inprocess) API  in Chainweb
module Chainweb.Test.Pact.Utils
( -- * test data
  someED25519Pair
, genesis
, chainId0
, testPactFilesDir
, testKeyPairs
  -- * helper functions
, getByteString
, formatB16PubKey
, mkPactTestTransactions
, mkPactTransaction
) where


import Data.Aeson (Value, object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.Default (def)
import Data.Text (Text, pack)
import Data.Traversable (for)
import Data.Word (Word64)

-- internal pact modules

import Pact.ApiReq (ApiKeyPair(..), mkKeyPairs)
import Pact.Types.Command (PublicMeta(..), mkCommand)
import Pact.Types.Crypto (SomeKeyPair, PublicKeyBS(..), PrivateKeyBS(..),
                          PPKScheme(..), formatPublicKey)
import Pact.Types.RPC (PactRPC(Exec), ExecMsg(..))
import Pact.Types.Util (toB16Text)

-- internal chainweb modules

import Chainweb.BlockHeader (BlockHeader)
import Chainweb.ChainId (ChainId, testChainId)
import Chainweb.Pact.Types (PactTransaction(..))
import Chainweb.Test.Utils (toyGenesis)


testKeyPairs :: IO [SomeKeyPair]
testKeyPairs = do
    let (pub, priv, addr, scheme) = someED25519Pair
        apiKP = ApiKeyPair priv (Just pub) (Just addr) (Just scheme)
    mkKeyPairs [apiKP]

testPactFilesDir :: String
testPactFilesDir = "test/config/"

someED25519Pair :: (PublicKeyBS, PrivateKeyBS, Text, PPKScheme)
someED25519Pair =
    ( PubBS $ getByteString
        "ba54b224d1924dd98403f5c751abdd10de6cd81b0121800bf7bdbdcfaec7388d"
    , PrivBS $ getByteString
        "8693e641ae2bbe9ea802c736f42027b03f86afe63cae315e7169c9c496c17332"
    , "ba54b224d1924dd98403f5c751abdd10de6cd81b0121800bf7bdbdcfaec7388d"
    , ED25519
    )

genesis :: BlockHeader
genesis = toyGenesis chainId0

chainId0 :: ChainId
chainId0 = testChainId 0

------------------------------------------------------------------------------
-- helper logic
------------------------------------------------------------------------------

getByteString :: ByteString -> ByteString
getByteString = fst . B16.decode

formatB16PubKey :: SomeKeyPair -> Text
formatB16PubKey = toB16Text . formatPublicKey

mkPactTestTransactions :: [String] -> IO [PactTransaction]
mkPactTestTransactions cmdStrs = do
    kps <- testKeyPairs
    let theData = object ["test-admin-keyset" .= fmap formatB16PubKey kps]
    let intSeq = [0, 1 ..] :: [Word64]
    -- using 1 as the nonce here so the hashes match for the same commands (for testing only)
    for (intSeq `zip` cmdStrs) $ \(i,c) ->
      mkPactTransaction kps theData "1" i c
  where

mkPactTransaction
  :: [SomeKeyPair]
  -> Value
  -> Text
  -> Word64
  -> String
  -> IO PactTransaction
mkPactTransaction keyPairs theData nonce txId theCode = do
    let pubMeta = def :: PublicMeta
    cmd <- mkCommand keyPairs pubMeta nonce $
      Exec (ExecMsg (pack theCode) theData)
    pure $ PactTransaction txId cmd
