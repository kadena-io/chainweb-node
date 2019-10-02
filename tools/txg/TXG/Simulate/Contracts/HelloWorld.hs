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

module TXG.Simulate.Contracts.HelloWorld where

import Data.Aeson
import Data.Default
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
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
import Pact.Types.Command (Command(..), SomeKeyPairCaps)

-- chainweb

import TXG.Simulate.Utils


helloWorldContractLoader
    :: PublicMeta
    -> NonEmpty SomeKeyPairCaps
    -> IO (Command Text)
helloWorldContractLoader meta adminKS = do
  let theData = object ["admin-keyset" .= fmap (formatB16PubKey . fst) adminKS]
  mkExec (T.unpack theCode) theData meta (NEL.toList adminKS) Nothing Nothing
  where
    theCode = [text|
(module helloWorld 'admin-keyset
  "A smart contract to greet the world."
  (defun hello (name)
    "Do the hello-world dance"
    (format "Hello {}!" [name])))
|]

newtype Name = Name { getName :: Text }
    deriving (Eq, Show, Generic)

instance Fake Name where
  fake = Name <$> personName

helloRequest :: Name -> IO (Command Text)
helloRequest (Name name) = mkExec theCode theData def [] Nothing Nothing
  where
    theData = Null
    theCode = printf "(helloWorld.hello \"%s\")" name
