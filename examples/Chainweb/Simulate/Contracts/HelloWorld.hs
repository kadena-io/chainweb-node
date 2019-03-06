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
import qualified Data.Text as T
-- import Data.ByteString (ByteString)

import Fake
import Fake.Provider.Person.EN_US

import GHC.Generics

import NeatInterpolation

import Text.Printf (printf)

-- pact

import Pact.ApiReq (KeyPair(..), mkExec)
import Pact.Types.Command (Command (..))
-- import Pact.Types.Crypto (PPKScheme)
-- import Pact.Types.RPC (PactRPC(..), ExecMsg(..))

-- chainweb

-- import Chainweb.Simulate.Utils

{-
   ;; Keysets cannot be created in code, thus we read them in
;; from the load message data.
(define-keyset '$keyset (read-keyset "$keyset"))

-}

helloWorldContractLoader :: [KeyPair] -> IO (Command Text)
helloWorldContractLoader  adminKeyset =
  mkExec (T.unpack theCode) theData def adminKeyset Nothing
  where
    theData = object ["admin-keyset" .= adminKeyset]
    theCode = [text| ;;
;; "Hello, world!" smart contract/module

(define-keyset 'admin-keyset (read-keyset 'admin-keyset))

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

helloRequest :: Name -> IO (Command Text)
helloRequest (Name name) = mkExec theCode theData def [] Nothing
  where
    theData = Null
    theCode = printf "(helloWorld.hello \"%s\")" name
