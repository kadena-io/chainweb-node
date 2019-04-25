{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Module: Main
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Emmanuel Denloye-Ito <emamnuel@kadena.io>
-- Stability: experimental
--
-- TODO
--

module Chainweb.Simulate.Contracts.HelloWorld where

import Data.Aeson
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T

import Fake
import Fake.Provider.Person.EN_US

import GHC.Generics

import NeatInterpolation

import Text.Printf (printf)

-- pact

import Pact.ApiReq (mkExec)
import Pact.Types.ChainMeta (PublicMeta(..))
import Pact.Types.Command (Command(..))
import Pact.Types.Crypto (SomeKeyPair)

-- chainweb

import Chainweb.Simulate.Utils

helloWorldContractLoader :: PublicMeta -> [SomeKeyPair] -> IO (Command Text)
helloWorldContractLoader meta adminKeyset = do
  let theData = object ["admin-keyset" .= fmap formatB16PubKey adminKeyset]
  mkExec (T.unpack theCode) theData meta adminKeyset Nothing
  where
    theCode = [text|
(module helloWorld 'admin-keyset
  "A smart contract to greet the world."
  (defun hello (name)
    "Do the hello-world dance"
    (format "Hello {}!" [name])))
|]

newtype Name = Name {getName :: Text} deriving (Eq, Show, Generic)

instance Fake Name where
  fake = Name <$> personName

helloRequest :: Name -> IO (Command Text)
helloRequest (Name name) = mkExec theCode theData def [] Nothing
  where
    theData = Null
    theCode = printf "(helloWorld.hello \"%s\")" name
