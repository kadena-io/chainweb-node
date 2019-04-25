{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# language GeneralizedNewtypeDeriving #-}

-- |
module Chainweb.Simulate.Contracts.Common where

import Control.Monad.Catch

import Data.Aeson
import Data.Attoparsec.ByteString.Char8
import Data.Bifunctor (bimap)
import Data.Char
import Data.Decimal
import Data.List (uncons)
import Data.Maybe
import Data.Text (Text)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import Fake

import GHC.Generics hiding (from, to)

-- -- import System.Random

import Text.Printf

-- PACT

import Pact.ApiReq (mkExec)
import Pact.Parse (ParsedInteger(..),ParsedDecimal(..))
import Pact.Types.Command (Command(..))
import Pact.Types.Crypto (SomeKeyPair, defaultScheme, genKeyPair)
import Pact.Types.ChainMeta (PublicMeta(..))

-- CHAINWEB

import Chainweb.Simulate.Utils
import Chainweb.ChainId
import Chainweb.Utils

createPaymentsAccount :: PublicMeta -> String -> IO ([SomeKeyPair], Command Text)
createPaymentsAccount meta name = do
    adminKeyset <- testSomeKeyPairs
    nameKeyset <- return <$> genKeyPair defaultScheme
    let theData = object [T.pack (name ++ "-keyset") .= fmap formatB16PubKey nameKeyset]
    res <- mkExec theCode theData meta adminKeyset Nothing
    return (nameKeyset, res)
  where
    theCode = printf "(payments.create-account \"%s\" %s (read-keyset \"%s-keyset\"))" name (show (1000000.1 :: Decimal)) name


createCoinAccount :: PublicMeta -> String -> IO ([SomeKeyPair], Command Text)
createCoinAccount meta name = do
  adminKeyset <- testSomeKeyPairs
  nameKeyset <- return <$> genKeyPair defaultScheme
  let theData = object [T.pack (name ++ "-keyset") .= fmap formatB16PubKey nameKeyset]
  res <- mkExec theCode theData meta adminKeyset Nothing
  return (nameKeyset, res)
  where
    theCode = printf "(coin.create-account \"%s\" (read-keyset \"%s\"))" name name

createPaymentsAccounts :: PublicMeta -> IO [([SomeKeyPair], Command Text)]
createPaymentsAccounts meta = traverse (createPaymentsAccount meta . safeCapitalize) (words names)

createCoinAccounts :: PublicMeta -> IO [([SomeKeyPair], Command Text)]
createCoinAccounts meta = traverse (createCoinAccount meta . safeCapitalize) (words names)

safeCapitalize :: String -> String
safeCapitalize = fromMaybe [] . fmap (uncurry (:) . bimap toUpper (map toLower)) . uncons

names :: String
names = "mary elizabeth patricia jennifer linda barbara margaret susan dorothy jessica james john robert michael william david richard joseph charles thomas"

newtype Account = Account
  { getAccount :: String
  } deriving (Eq, Ord, Show, Generic)

accountNames :: [Account]
accountNames = map (Account . safeCapitalize) (words names)

instance Fake Account where
  fake = elements accountNames

newtype Amount = Amount
  { getAmount :: Decimal
  } deriving (Eq, Show, Generic)

instance Fake Amount where
  fake =
    Amount <$>
    (realFracToDecimal <$> fromRange (0, 10) <*>
     (fromRange (lowerLimit, upperLimit) :: FGen Double))
    where
      lowerLimit = 0
      upperLimit = 1000

newtype Balance = Balance
  { getBalance :: Decimal
  } deriving (Eq, Show, Generic)

instance Fake Balance where
  fake = Balance . fromIntegral <$> fromRange (0, 100000 :: Integer)

distinctPair :: (Fake a, Eq a) => FGen (a,a)
distinctPair = fake >>= \a -> (,) a <$> suchThat fake (/= a)

parens :: String -> String
parens s = "(" ++ s ++ ")"

-- hardcoded sender (sender00)
makeMeta :: ChainId -> PublicMeta
makeMeta cid = PublicMeta (chainIdToText cid) "sender00" (ParsedInteger 100) (ParsedDecimal 0.0001)

newtype ContractName = ContractName { getContractName :: String}
  deriving (Eq, Ord, Show, Generic)
  deriving newtype Read

instance ToJSON ContractName

instance FromJSON ContractName

parseBytes :: MonadThrow m => Text -> Parser a -> B8.ByteString -> m a
parseBytes name parser b = either (throwM . TextFormatException . msg) return $ parseOnly (parser <* endOfInput) b
  where
    msg e = "Failed to parse " <> sshow b <> " as " <> name <> ": "
        <> T.pack e
