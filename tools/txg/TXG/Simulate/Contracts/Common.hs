{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module TXG.Simulate.Contracts.Common
  ( -- * Types
    Account(..)
  , Amount(..)
  , Balance(..)
  , ContractName(..)
  , makeMeta
  , makeMetaWithSender
    -- * Accounts
  , accountNames
  , coinAccountNames
  , createPaymentsAccounts
  , createCoinAccount
  , createCoinAccounts
    -- * Generation
  , distinctPair
  , distinctPairSenders
    -- * Parsing
  , parseBytes
    -- * Utils
  , stockKey
  ) where

import Control.Lens hiding ((.=), elements, uncons)
import Control.Monad.Catch

import Data.Aeson
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Char
import Data.Decimal
import Data.FileEmbed
import Data.List (uncons)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Yaml as Y
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T


import Fake

import GHC.Generics hiding (from, to)

import Text.Printf

-- PACT

import Pact.ApiReq (mkExec, ApiKeyPair(..), mkKeyPairs)
import Pact.Parse
import qualified Pact.Types.ChainId as CM
import qualified Pact.Types.ChainMeta as CM
import Pact.Types.Command (Command(..), SomeKeyPairCaps)
import Pact.Types.Crypto

-- CHAINWEB

import Chainweb.ChainId
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Version
import TXG.Simulate.Utils



createPaymentsAccount
    :: ChainwebVersion
    -> CM.PublicMeta
    -> String
    -> IO (NonEmpty SomeKeyPairCaps, Command Text)
createPaymentsAccount v meta name = do
    adminKS <- testSomeKeyPairs
    nameKeyset <- (\k -> [(k, [])]) <$> genKeyPair defaultScheme
    let theData = object
          [ T.pack (name ++ "-keyset") .= fmap (formatB16PubKey . fst) nameKeyset
          ]
    res <- mkExec theCode theData meta (NEL.toList adminKS) (Just $ CM.NetworkId $ toText v) Nothing
    pure (NEL.fromList nameKeyset, res)
  where
    theCode = printf "(payments.create-account \"%s\" %s (read-keyset \"%s-keyset\"))" name (show (1000000.1 :: Decimal)) name

createCoinAccount
    :: ChainwebVersion
    -> CM.PublicMeta
    -> String
    -> IO (NonEmpty SomeKeyPairCaps, Command Text)
createCoinAccount v meta name = do
    adminKS <- testSomeKeyPairs
    nameKeyset <- NEL.fromList <$> getKeyset
    let theData = object [T.pack (name ++ "-keyset") .= fmap (formatB16PubKey . fst) nameKeyset]
    res <- mkExec theCode theData meta (NEL.toList adminKS) (Just $ CM.NetworkId $ toText v) Nothing
    pure (nameKeyset, res)
  where
    theCode = printf "(coin.create-account \"%s\" (read-keyset \"%s\"))" name name
    isSenderAccount name'  =
      elem name' (map getAccount coinAccountNames)

    getKeyset :: IO [SomeKeyPairCaps]
    getKeyset
      | isSenderAccount name = do
          keypair <- stockKey (T.pack name)
          mkKeyPairs [keypair]
      | otherwise = (\k -> [(k, [])]) <$> genKeyPair defaultScheme

createPaymentsAccounts :: ChainwebVersion -> CM.PublicMeta -> IO (NonEmpty (NonEmpty SomeKeyPairCaps, Command Text))
createPaymentsAccounts v meta = traverse (createPaymentsAccount v meta) names

createCoinAccounts :: ChainwebVersion -> CM.PublicMeta -> IO (NonEmpty (NonEmpty SomeKeyPairCaps, Command Text))
createCoinAccounts v meta = traverse (createCoinAccount v meta) names

coinAccountNames :: [Account]
coinAccountNames = (Account . ("sender0" <>) . show) <$> [0 :: Int .. 9]

stockKeyFile :: ByteString
stockKeyFile = $(embedFile "pact/genesis/testnet/keys.yaml")

-- | Convenient access to predefined testnet sender accounts
stockKey :: Text -> IO ApiKeyPair
stockKey s = do
  let Right (Object o) = Y.decodeEither' stockKeyFile
      Just (Object kp) = HM.lookup s o
      Just (String pub) = HM.lookup "public" kp
      Just (String priv) = HM.lookup "secret" kp
      mkKeyBS = decodeKey . encodeUtf8
  return $ ApiKeyPair (PrivBS $ mkKeyBS priv) (Just $ PubBS $ mkKeyBS pub) Nothing (Just ED25519) Nothing

safeCapitalize :: String -> String
safeCapitalize = fromMaybe [] . fmap (uncurry (:) . bimap toUpper (map toLower)) . uncons

names :: NonEmpty String
names = NEL.map safeCapitalize . NEL.fromList $ words "mary elizabeth patricia jennifer linda barbara margaret susan dorothy jessica james john robert michael william david richard joseph charles thomas"

newtype Account = Account { getAccount :: String } deriving (Eq, Ord, Show, Generic)

accountNames :: NonEmpty Account
accountNames = NEL.map Account names

instance Fake Account where
  fake = elements $ NEL.toList accountNames

newtype Amount = Amount
  { getAmount :: Decimal
  } deriving (Eq, Show, Generic)

instance Fake Amount where
  fake =
    (Amount . realFracToDecimal 12) <$>
    (fromRange @Double (lowerLimit, upperLimit))
    where
      lowerLimit = 0
      upperLimit = 5

newtype Balance = Balance
  { getBalance :: Decimal
  } deriving (Eq, Show, Generic)

instance Fake Balance where
  fake = Balance . fromIntegral <$> fromRange (0, 100000 :: Integer)

distinctPair :: (Fake a, Eq a) => FGen (a,a)
distinctPair = fake >>= \a -> (,) a <$> suchThat fake (/= a)

distinctPairSenders :: FGen (Account, Account)
distinctPairSenders = fakeInt 0 9 >>= go
  where
    append num = Account $ "sender0" ++ show num
    go n = do
      m <- fakeInt 0 9
      if n == m then go n else return (append n, append m)

-- hardcoded sender (sender00)
makeMeta :: ChainId -> IO CM.PublicMeta
makeMeta cid = do
    t <- toTxCreationTime <$> getCurrentTimeIntegral
    return $ CM.PublicMeta
        {
          CM._pmChainId = CM.ChainId $ toText cid
        , CM._pmSender = "sender00"
        , CM._pmGasLimit = 10000
        , CM._pmGasPrice = 0.001
        , CM._pmTTL = 3600
        , CM._pmCreationTime = t
        }

makeMetaWithSender :: String -> ChainId -> IO CM.PublicMeta
makeMetaWithSender sender cid =
    set CM.pmSender (T.pack sender) <$> makeMeta cid

newtype ContractName = ContractName { getContractName :: String }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype Read

instance ToJSON ContractName

instance FromJSON ContractName

parseBytes :: MonadThrow m => Text -> Parser a -> B8.ByteString -> m a
parseBytes name parser b = either (throwM . TextFormatException . msg) pure $ parseOnly (parser <* endOfInput) b
  where
    msg e = "Failed to parse " <> sshow b <> " as " <> name <> ": "
        <> T.pack e

toTxCreationTime :: Time Integer -> CM.TxCreationTime
toTxCreationTime (Time timespan) = case timeSpanToSeconds timespan of
    Seconds s  -> CM.TxCreationTime $ ParsedInteger s
