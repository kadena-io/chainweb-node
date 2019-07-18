{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module TXG.Simulate.Contracts.Common
  ( -- * Types
    Account(..)
  , Amount(..)
  , Balance(..)
  , ContractName(..)
  , makeMeta
    -- * Accounts
  , accountNames
  , createPaymentsAccounts
  , createCoinAccounts
    -- * Generation
  , distinctPair
    -- * Parsing
  , parseBytes
  ) where

import Control.Monad.Catch

import Data.Aeson
import Data.Attoparsec.ByteString.Char8
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Char8 as B8
import Data.Char
import Data.Decimal
import Data.List (uncons)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Fake

import GHC.Generics hiding (from, to)

import Text.Printf

-- PACT

import Pact.ApiReq (mkExec)
import qualified Pact.Types.ChainId as CM
import qualified Pact.Types.ChainMeta as CM
import Pact.Types.Command (Command(..))
import Pact.Types.Crypto (SomeKeyPair, defaultScheme, genKeyPair)

-- CHAINWEB

import Chainweb.ChainId
import Chainweb.Utils
import TXG.Simulate.Utils

createPaymentsAccount :: CM.PublicMeta -> String -> IO (NonEmpty SomeKeyPair, Command Text)
createPaymentsAccount meta name = do
    adminKS <- testSomeKeyPairs
    nameKeyset <- pure <$> genKeyPair defaultScheme
    let theData = object [T.pack (name ++ "-keyset") .= fmap formatB16PubKey nameKeyset]
    res <- mkExec theCode theData meta (NEL.toList adminKS) Nothing
    pure (nameKeyset, res)
  where
    theCode = printf "(payments.create-account \"%s\" %s (read-keyset \"%s-keyset\"))" name (show (1000000.1 :: Decimal)) name


createCoinAccount :: CM.PublicMeta -> String -> IO (NonEmpty SomeKeyPair, Command Text)
createCoinAccount meta name = do
  adminKS <- testSomeKeyPairs
  nameKeyset <- pure <$> genKeyPair defaultScheme
  let theData = object [T.pack (name ++ "-keyset") .= fmap formatB16PubKey nameKeyset]
  res <- mkExec theCode theData meta (NEL.toList adminKS) Nothing
  pure (nameKeyset, res)
  where
    theCode = printf "(coin.create-account \"%s\" (read-keyset \"%s\"))" name name

createPaymentsAccounts :: CM.PublicMeta -> IO (NonEmpty (NonEmpty SomeKeyPair, Command Text))
createPaymentsAccounts meta = traverse (createPaymentsAccount meta) names

createCoinAccounts :: CM.PublicMeta -> IO (NonEmpty (NonEmpty SomeKeyPair, Command Text))
createCoinAccounts meta = traverse (createCoinAccount meta) names

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

-- hardcoded sender (sender00)
makeMeta :: ChainId -> CM.PublicMeta
makeMeta cid = CM.PublicMeta (CM.ChainId $ toText cid) "sender00" 100 1.0

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
