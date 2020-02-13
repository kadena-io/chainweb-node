{-# LANGUAGE OverloadedStrings #-}

module TXG.Simulate.Utils where

import Control.Monad.IO.Class

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)

import Pact.ApiReq (ApiKeyPair(..), mkExec, mkKeyPairs)
import Pact.Types.ChainMeta (PublicMeta(..))
import Pact.Types.ChainId (NetworkId(..))
import Pact.Types.Command (Command(..), SomeKeyPairCaps)
import Pact.Types.Crypto
    (PPKScheme(..), PrivateKeyBS(..), PublicKeyBS(..), SomeKeyPair,
    formatPublicKey)
import Pact.Types.Util (toB16Text)

import Chainweb.Version
import Chainweb.Utils.Text

testApiKeyPairs :: NonEmpty ApiKeyPair
testApiKeyPairs =
  let (pub, priv, addr, scheme) = someED25519Pair
      apiKP = ApiKeyPair priv (Just pub) (Just addr) (Just scheme) Nothing
   in pure apiKP

testSomeKeyPairs :: IO (NonEmpty SomeKeyPairCaps)
testSomeKeyPairs = do
    let (pub, priv, addr, scheme) = someED25519Pair
        apiKP = ApiKeyPair priv (Just pub) (Just addr) (Just scheme) Nothing
    NEL.fromList <$> mkKeyPairs [apiKP]

formatB16PubKey :: SomeKeyPair -> Text
formatB16PubKey = toB16Text . formatPublicKey

-- | note this is "sender00"'s key
someED25519Pair :: (PublicKeyBS, PrivateKeyBS, Text, PPKScheme)
someED25519Pair =
    ( PubBS $ decodeKey
        "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"
    , PrivBS $ decodeKey
        "251a920c403ae8c8f65f59142316af3c82b631fba46ddea92ee8c95035bd2898"
    , "368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca"
    , ED25519
    )

-- someED25519Pair :: (PublicKeyBS, PrivateKeyBS, Text, PPKScheme)
-- someED25519Pair =
--     ( PubBS $ decodeKey
--         "ba54b224d1924dd98403f5c751abdd10de6cd81b0121800bf7bdbdcfaec7388d"
--     , PrivBS $ decodeKey
--         "8693e641ae2bbe9ea802c736f42027b03f86afe63cae315e7169c9c496c17332"
--     , "ba54b224d1924dd98403f5c751abdd10de6cd81b0121800bf7bdbdcfaec7388d"
--     , ED25519
--     )

decodeKey :: ByteString -> ByteString
decodeKey = fst . B16.decode

initAdminKeysetContract
    :: ChainwebVersion
    -> PublicMeta
    -> NonEmpty SomeKeyPairCaps
    -> IO (Command Text)
initAdminKeysetContract v meta adminKS =
  mkExec theCode theData meta (NEL.toList adminKS) (Just $ NetworkId $ toText v) Nothing
  where
    theCode = "(define-keyset 'admin-keyset (read-keyset \"admin-keyset\"))"
    theData = object ["admin-keyset" .= fmap (formatB16PubKey . fst) adminKS]

measureDiffTime :: MonadIO m => m a -> m (NominalDiffTime, a)
measureDiffTime someaction = do
  start <- liftIO getCurrentTime
  result <- someaction
  end <- liftIO getCurrentTime
  return (diffUTCTime end start, result)
