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

import Data.Text (Text)
import NeatInterpolation

{-
   ;; Keysets cannot be created in code, thus we read them in
;; from the load message data.
(define-keyset '$keyset (read-keyset "$keyset"))

-}

helloWorldContract keyset =
                   [text| ;;
;; "Hello, world!" smart contract/module

;; Define the module.
(module helloWorld 'admin-keyset
  "A smart contract to greet the world."
  (defun hello (name)
    "Do the hello-world dance"
    (format "Hello {}!" [name]))
)
|]

hello = [text|
  (defun hello (name)
    "Do the hello-world dance"
    (format "Hello {}!" [name]))
|]
