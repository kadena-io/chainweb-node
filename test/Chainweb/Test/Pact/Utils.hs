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

import Control.Lens ((.~))

import Data.Aeson (Value, object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.Default
import Data.Maybe
import Data.Text (Text, pack)
import Data.Vector (Vector)
import qualified Data.Vector as V

-- internal pact modules

import Pact.ApiReq (ApiKeyPair(..), mkKeyPairs)
import Pact.Types.Command
import Pact.Types.Crypto
import Pact.Types.RPC (PactRPC(Exec), ExecMsg(..))
import Pact.Types.Util (toB16Text)

-- internal chainweb modules

import Chainweb.BlockHeader (BlockHeader)
import Chainweb.ChainId (ChainId, testChainId)
import Chainweb.Test.Utils (toyGenesis)
import Chainweb.Transaction

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

mkPactTestTransactions :: Vector String -> IO (Vector ChainwebTransaction)
mkPactTestTransactions cmdStrs = do
    kps <- testKeyPairs
    let theData = object ["test-admin-keyset" .= fmap formatB16PubKey kps]
    -- using 1 as the nonce here so the hashes match for the same commands (for testing only)
    vMaybes <- traverse (mkPactTransaction kps theData "1") cmdStrs
    return $ V.map fromJust (V.filter isJust vMaybes)

mkPactTransaction
  :: [SomeKeyPair]
  -> Value
  -> Text
  -> String
  -> IO (Maybe ChainwebTransaction)
mkPactTransaction keyPairs theData nonce theCode = do
    let pubMeta = pmSender .~ "sender0" $ def
    cmdBS <- mkCommand keyPairs pubMeta nonce $
        Exec (ExecMsg (pack theCode) theData)
    return $ case verifyCommand cmdBS of
        ProcSucc cmd -> Just $ (\bs -> PayloadWithText bs (_cmdPayload cmd)) <$> cmdBS
        ProcFail{} -> Nothing
