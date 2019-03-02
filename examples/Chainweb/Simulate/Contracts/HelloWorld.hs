{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
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
import Data.ByteString (ByteString)

import Fake
import Fake.Provider.Person.EN_US

import GHC.Generics

import NeatInterpolation

-- pact

import Pact.ApiReq (KeyPair(..))
import Pact.Types.Command (mkCommand, Command (..), PublicMeta)
import Pact.Types.Crypto (PPKScheme(..))
import Pact.Types.RPC (PactRPC(..), ExecMsg(..))

-- chainweb

import Chainweb.Simulate.Utils

{-
   ;; Keysets cannot be created in code, thus we read them in
;; from the load message data.
(define-keyset '$keyset (read-keyset "$keyset"))

-}

helloWorldContractLoader :: Nonce -> [KeyPair] -> Command ByteString
helloWorldContractLoader (Nonce nonce) adminKeyset =
  mkCommand madeKeyset (def :: PublicMeta) nonce (Exec (ExecMsg theCode theData))
  where
    madeKeyset = map (\(KeyPair sec pub) -> (ED25519, sec, pub)) adminKeyset
    theData = object ["admin-keyset" .= adminKeyset]
    theCode = [text| ;;
;; "Hello, world!" smart contract/module

;; Define the module.
(module helloWorld 'admin-keyset
  "A smart contract to greet the world."
  (defun hello (name)
    "Do the hello-world dance"
    (format "Hello {}!" [name]))
)
|]

newtype Name = Name {getName :: Text} deriving (Eq, Show, Generic)

instance Fake Name where
  fake = Name <$> personName

helloRequest :: Nonce -> Name -> Command ByteString
helloRequest (Nonce nonce) (Name name) =
  mkCommand [] (def :: PublicMeta) nonce (Exec (ExecMsg theCode theData))
  where
    theData = Null
    theCode = [text|(hello "$name")|]
