{-# language OverloadedStrings #-}
{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language QuasiQuotes #-}
{-# language LambdaCase #-}

-- |

module Chainweb.Simulate.Contracts.CryptoCritters where

-- import Control.Monad
import Control.Monad.Zip

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Maybe
-- import qualified Data.Text as T
import Data.Text (Text)

-- import Fake

import GHC.Generics hiding (from, to)

import NeatInterpolation

-- import System.Random

-- import Text.Printf

-- pact
import Pact.ApiReq
import Pact.Types.Crypto
-- import Pact.Types.Command
-- import Pact.Types.RPC

cryptoCritterContract :: Text -> Text
cryptoCritterContract adminKeyset = [text|
(module critters '$adminKeyset
  "Collectible Crypto Critters"
  (defschema critter
    "Data defining a critter"
    genes:string
    matron-id:integer
    sire-id:integer
    generation:integer
    owner:keyset
    transferring:bool
    transfer-to:keyset
    available-to-breed:bool
  )

  (defschema countSchema
    count:integer
  )

  (deftable critters:{critter})
  (deftable countTable:{countSchema})

  (defun get-inc-count:string (k:string)
    "Incremenent row K in the count table"
    (with-read countTable k {"count" := count}
     (write countTable k {"count": (+ count 1)})
     (format "{}" [count])
    )
  )

  (defun create-critter:integer (genes:string)
    "Create a gen0 critter using GENES"
    (enforce-keyset '$adminKeyset)
    (let ((id (get-inc-count "critters")))
      (insert critters id
        { "matron-id": 0,
          "sire-id": 0,
          "generation": 0,
          "genes": genes,
          "owner": (read-keyset "$adminKeyset"),
          "transferring": false,
          "transfer-to": (read-keyset "$adminKeyset"),
          "available-to-breed": false
        }
      )
      id
    )
  )

  (defun show-critter:string (suffix:string critter:object{critter})
    "String representation of CRITTER appending SUFFIX"
    (bind critter { "matron-id" := m,
              "sire-id" := s,
              "generation" := gen,
              "genes" := genes,
              "owner" := o,
              "transferring" := t,
              "transfer-to" := tto
            }
      (+ (format "gen: {} matron: {} sire: {} owner: {} {} {} {}\n"
          [gen m s o t tto genes]) suffix)
    )
  )

  (defun show-generation:string (gen:integer)
    "Get all the critters in GEN generation"
    (let ((cs (select critters (where 'generation (= gen)))))
         (fold (show-critter) "" cs)
    )
  )

  (defun owner (critter-id:string)
    "Get the owner of a critter CRITTER-ID"
    (with-read critters critter-id {"owner":= o} o)
  )

  (defun transfer-critter (new-owner:keyset critter-id:string)
    "Transfer critter CRITTER-ID ownership to NEW-OWNER (Note: UNSAFE!)"
    (let ((c (read critters critter-id)))
        (enforce-keyset (at "owner" c))
        (update critters critter-id
          (+ {"owner": new-owner} c)
        )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Safe critter transfers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun set-transfer (critter-id:string transfer:bool to:keyset)
    "Set critter CRITTER-ID TRANSFER flag to TO keyset"
    ;; NOTE: This is a private helper function
    (let ((c (read critters critter-id)))
        (enforce-keyset (at "owner" c))
        (update critters critter-id
          (+ {"transferring": transfer, "transfer-to": to} c)
        )
    )
  )

  (defun initiate-transfer (new-owner:keyset critter-id:string)
    "Transfer critter CRITTER-ID ownership to NEW-OWNER safely without \
    \the possibility of the critter getting lost"
    (let ((c (read critters critter-id)))
      (enforce-keyset (at "owner" c))
      ;; We don't call transferCritter because we're not the owner
      (set-transfer critter-id true new-owner)
    )
  )

  (defun complete-transfer (critter-id:string)
    (let ((c (read critters critter-id)))
      (enforce-keyset (at "transfer-to" c))
      ;; We don't call transferCritter because we're not the owner
      (update critters critter-id
        (+ {"owner": (at "transfer-to" c)} c)
      )
      (set-transfer critter-id false (read-keyset "$adminKeyset"))
    )
  )

  (defun cancel-transfer (critter-id:string)
    (let ((c (read critters critter-id)))
      (enforce-keyset (at "owner" c))
      ;; We don't call transferCritter because we're not the owner
      (set-transfer critter-id false (read-keyset "$adminKeyset"))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Critter breeding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun set-breeding (critter-id:string flag:bool)
    "Set a critter's breeding flag"
    ;; NOTE: This is a private helper function
    (let ((c (read critters critter-id)))
        (enforce-keyset (at "owner" c))
        (update critters critter-id
          (+ {"breeding": flag} c)
        )
    )
  )

  (defun solicit-mate (critter-id:string)
    "Make your critter available for breeding"
    (let ((c (read critters critter-id)))
      (enforce-keyset (at "owner" c))
      ;; We don't call transferCritter because we're not the owner
      (set-breeding critter-id true)
    )
  )

  (defun max (a b)
    (if (> a b) a b)
  )

  (defun combine-genes (a:string b:string)
    "Create child genes from two sets of parent genes"
    (let* ((ind 5)
          (left (take ind a))
          (right (drop ind b))
         )
      (+ left right)
    )
  )

  (defun breed (critter-id-a:string critter-id-b:string)
    "Make your critter available for breeding"
    (let ((a (read critters critter-id-a))
          (b (read critters critter-id-b))
         )
      (enforce-keyset (at "owner" b))
      (enforce (at "breeding" a) "That critter is not available for breeding")
      ;; We don't call transferCritter because we're not the owner
      (let ((i (get-inc-count "critters")))
        (insert critters (format "{}" [i])
          { "matronId": (format "{}" critter-id-a),
            "sireId": (format "{}" critter-id-b),
            "generation": (+ 1 (max (at "generation" a) (at "generation" b))),
            "genes": (combine-genes (at "genes" a) (at "genes" b)),
            "owner": (at "owner" a),
            "transferring": false,
            "transferTo": (read-keyset "$adminKeyset"),
            "available-to-breed": false
          }
        )
        i
      )
    )
  )

  (defun cancel (critter-id:string)
    "Take critter CRITTER-ID off the breeding market"
    (let ((c (read critters critter-id)))
      (enforce-keyset (at "owner" c))
      ;; We don't call transferCritter because we're not the owner
      (set-breeding critter-id false)
    )
  )
)
|]

{- example usage

(create-table critters)
(create-table countTable)
(insert countTable "critters" {"count":0})

-}

newtype Row = Row { getRow :: Text}
  deriving (Eq, Show, Generic)

newtype Genes = Genes { getGenes :: Text}
  deriving (Eq, Show, Generic)

newtype Critter = Critter { getCritter :: Value }
  deriving (Eq, Show, Generic)

newtype Generation = Generation { getGeneration :: Integer }
  deriving (Eq, Show, Generic)

newtype CritterId = CritterId { getCritterId :: Integer }
  deriving (Eq, Show, Generic)

newtype Suffix = Suffix
  { getSuffix :: Text
  } deriving (Eq, Show, Generic)

data CritterRequest
  = GetIncCount Row
  | CreateCritter Genes
  | ShowCritter Suffix
                Value
  | ShowGeneration Generation
  | Owner CritterId
  | TransferCritter [ApiKeyPair] CritterId
  | SetTransfer CritterId Bool [ApiKeyPair]
  | InitiateTransfer [ApiKeyPair] CritterId
  | CompleteTransfer CritterId
  | CancelTransfer CritterId
  | SetBreeding CritterId Bool
  | SolicitMate CritterId
  | CombineGenes Genes Genes
  | Breed CritterId CritterId
  | CancelBreed CritterId

createCritterRequest :: CritterRequest -> Text
createCritterRequest =
  \case
     GetIncCount _row -> undefined
     CreateCritter _genes -> undefined
     ShowCritter _suffix _critter -> undefined
     ShowGeneration _generation -> undefined
     Owner _critterid -> undefined
     TransferCritter _keyset _critterid -> undefined
     SetTransfer _critterid _transferflag _keyset -> undefined
     InitiateTransfer _keyset _critterid -> undefined
     CompleteTransfer _critterid -> undefined
     CancelTransfer _critterid -> undefined
     SetBreeding _critterid _flag -> undefined
     SolicitMate _critterid -> undefined
     CombineGenes _genesA _genesB -> undefined
     Breed _idA _idB -> undefined
     CancelBreed _critterid -> undefined

-- testAdminPrivates :: ByteString
-- testAdminPrivates =
--   "53108fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d2"

-- testAdminPublics :: ByteString
-- testAdminPublics =
--   "201a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c0dbc"

-- testPrivates :: [ByteString]
-- testPrivates =
--   [ "53108fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d3"
--   , "53108fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d4"
--   ]

-- testPublics :: [ByteString]
-- testPublics =
--   [ "201a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c1dbc"
--   , "201a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c2dbc"
--   ]

-- testAdminKeyPairs :: [KeyPair]
-- testAdminKeyPairs =
--   let mPair =
--         mzip (importPrivate testAdminPrivates) (importPublic testAdminPublics)
--       mKeyPair =
--         fmap (\(sec, pub) -> KeyPair {_kpSecret = sec, _kpPublic = pub}) mPair
--    in maybeToList mKeyPair

-- testKeyPairs :: [KeyPair]
-- testKeyPairs =
--   concat $
--   zipWith
--     (\private public ->
--        maybeToList $
--        KeyPair <$> importPrivate private <*> importPublic public)
--     testPrivates
--     testPublics
