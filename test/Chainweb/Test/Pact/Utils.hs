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
import Data.Maybe
import Data.Text (Text, pack)
import Data.Vector (Vector)
import qualified Data.Vector as V

-- internal pact modules

import Pact.ApiReq (ApiKeyPair(..), mkKeyPairs)
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Crypto
import Pact.Types.RPC (PactRPC(Exec), ExecMsg(..))
import Pact.Types.Util (toB16Text)
import Pact.Parse (ParsedDecimal(..),ParsedInteger(..))

-- internal chainweb modules

import Chainweb.BlockHeader (BlockHeader)
import Chainweb.ChainId (ChainId, accursedUnutterableChainId)
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
        "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"
    , PrivBS $ getByteString
        "251a920c403ae8c8f65f59142316af3c82b631fba46ddea92ee8c95035bd2898"
    , "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"
    , ED25519
    )

genesis :: BlockHeader
genesis = toyGenesis chainId0

chainId0 :: ChainId
chainId0 = accursedUnutterableChainId 0

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
    let pubMeta = PublicMeta "0" "sender00" (ParsedInteger 100) (ParsedDecimal 0.0001)
    cmdBS <- mkCommand keyPairs pubMeta nonce $
        Exec (ExecMsg (pack theCode) theData)
    return $ case verifyCommand cmdBS of
        ProcSucc cmd -> Just $ (\bs -> PayloadWithText bs (_cmdPayload cmd)) <$> cmdBS
        ProcFail{} -> Nothing
