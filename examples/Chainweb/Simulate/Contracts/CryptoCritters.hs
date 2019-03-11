{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

-- |

module Chainweb.Simulate.Contracts.CryptoCritters where

import Data.Aeson
import Data.Default
import qualified Data.Text as T
import Data.Text (Text)

import Fake (FGen)

import GHC.Generics hiding (from, to)

import NeatInterpolation (text)

import System.Random (StdGen, randomR)

-- pact
import Pact.ApiReq (mkExec)
import Pact.Types.Command (Command (..))
import Pact.Types.Crypto (SomeKeyPair)

-- chainweb

import Chainweb.Simulate.Utils

cryptoCritterContractLoader :: [SomeKeyPair] -> IO (Command Text)
cryptoCritterContractLoader adminKeyset = do
  let theData = object ["admin-keyset" .= fmap formatB16PubKey adminKeyset]
  mkExec (T.unpack theCode) theData def adminKeyset Nothing
  where
    theCode = [text|

(module critters 'admin-keyset
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
    (enforce-keyset 'admin-keyset)
    (let ((id (get-inc-count "critters")))
      (insert critters id
        { "matron-id": 0,
          "sire-id": 0,
          "generation": 0,
          "genes": genes,
          "owner": 'admin-keyset,
          "transferring": false,
          "transfer-to": 'admin-keyset,
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
      (set-transfer critter-id false 'admin-keyset)
    )
  )

  (defun cancel-transfer (critter-id:string)
    (let ((c (read critters critter-id)))
      (enforce-keyset (at "owner" c))
      ;; We don't call transferCritter because we're not the owner
      (set-transfer critter-id false 'admin-keyset)
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
            "transferTo": 'admin-keyset,
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

(create-table critters)

|]

{- example usage

(create-table critters)
(create-table countTable)
(insert countTable "critters" {"count":0})

-}

newtype Row = Row {getRow :: Text}
  deriving (Eq, Show, Generic)

newtype Genes = Genes {getGenes :: Text}
  deriving (Eq, Show, Generic)

newtype Critter = Critter {getCritter :: Value }
  deriving (Eq, Show, Generic)

newtype Generation = Generation {getGeneration :: Text }
  deriving (Eq, Show, Generic)

newtype CritterId = CritterId {getCritterId :: Text }
  deriving (Eq, Show, Generic)

newtype Suffix = Suffix
  {getSuffix :: Text
  } deriving (Eq, Show, Generic)

data CritterRequest
  = GetIncCount Row
  | CreateCritter Genes
  -- | ShowCritter Suffix Critter
  | ShowGeneration Generation
  | Owner CritterId
  | TransferCritter [SomeKeyPair] CritterId
  | SetTransfer CritterId Bool [SomeKeyPair]
  | InitiateTransfer [SomeKeyPair] CritterId
  | CompleteTransfer CritterId
  | CancelTransfer CritterId
  | SetBreeding CritterId Bool
  | SolicitMate CritterId
  | CombineGenes Genes Genes
  | Breed CritterId CritterId
  | CancelBreed CritterId

_createCritterRequest :: CritterRequest -> IO (Command Text)
_createCritterRequest = undefined


--------------------------------
-- THIS NEEDS TO BE REWRITTEN --
--------------------------------

-- createCritterRequest :: Nonce -> CritterRequest -> Command ByteString
-- createCritterRequest (Nonce nonce) (GetIncCount (Row row)) =
--   mkCommand madeKeyset (def :: PublicMeta) nonce (Exec (ExecMsg theCode theData))
--   where
--     madeKeyset = []
--     theCode = [text|(get-inc-count $row)|]
--     theData = Null
-- createCritterRequest (Nonce nonce) (CreateCritter (Genes genes)) =
--   mkCommand madeKeyset (def :: PublicMeta) nonce (Exec (ExecMsg theCode theData))
--   where
--     madeKeyset = []
--     theCode = [text|(create-critter $genes)|]
--     theData = Null

-----------------------------------------------
-- TODO: How do we interpolate a JSON value? --
-----------------------------------------------
-- createCritterRequest (Nonce nonce) (ShowCritter (Suffix suffix) (Critter critter)) =
--   mkCommand madeKeyset (def :: PublicMeta) nonce (Exec (ExecMsg theCode theData))
--   where
--     madeKeyset = []
--     theCode = [text|(show-critter $suffix $something)|]
--     theData = object undefined
--     something = undefined critter

-- createCritterRequest (Nonce nonce) (ShowGeneration (Generation generation)) =
--   mkCommand madeKeyset (def :: PublicMeta) nonce (Exec (ExecMsg theCode theData))
--   where
--     madeKeyset = []
--     theCode = [text|(show-generation $generation)|]
--     theData = Null
-- createCritterRequest (Nonce nonce) (Owner (CritterId critterid)) =
--   mkCommand madeKeyset (def :: PublicMeta) nonce (Exec (ExecMsg theCode theData))
--   where
--     madeKeyset = []
--     theCode = [text|(owner $critterid)|]
--     theData = Null
-- createCritterRequest (Nonce nonce) (TransferCritter keyset (CritterId critterid)) =
--   mkCommand madeKeyset (def :: PublicMeta) nonce (Exec (ExecMsg theCode theData))
--   where
--     madeKeyset = map (\(KeyPair sec pub) -> (ED25519, sec, pub)) keyset
--     theCode = [text|(transfer-critter (read-keyset 'transfer-critter-keyset) $critterid)|]
--     theData = object ["transfer-critter-keyset" .= keyset]
-- createCritterRequest (Nonce nonce) (SetTransfer (CritterId critterid) transferflag keyset) =
--   mkCommand madeKeyset (def :: PublicMeta) nonce (Exec (ExecMsg theCode theData))
--   where
--     madeKeyset = map (\(KeyPair sec pub) -> (ED25519, sec, pub)) keyset
--     theCode = [text|(set-transfer $critterid $flag (read-keyset 'set-transfer-keyset))|]
--     theData = object ["set-transfer-keyset" .= keyset]
--     flag = if transferflag
--               then "true"
--               else "false"
-- createCritterRequest (Nonce nonce) (InitiateTransfer keyset (CritterId critterid)) =
--   mkCommand madeKeyset (def :: PublicMeta) nonce (Exec (ExecMsg theCode theData))
--   where
--     madeKeyset = map (\(KeyPair sec pub) -> (ED25519, sec, pub)) keyset
--     theCode = [text|(initiate-transfer (read-keyset 'initiate-transfer-keyset) $critterid)|]
--     theData = object ["initiate-transfer-keyset" .= keyset]
-- createCritterRequest (Nonce nonce) (CompleteTransfer (CritterId critterid)) =
--   mkCommand madeKeyset (def :: PublicMeta) nonce (Exec (ExecMsg theCode theData))
--   where
--     madeKeyset = []
--     theCode = [text|(complete-transfer $critterid)|]
--     theData = Null
-- createCritterRequest (Nonce nonce) (CancelTransfer (CritterId critterid)) =
--   mkCommand madeKeyset (def :: PublicMeta) nonce (Exec (ExecMsg theCode theData))
--   where
--     madeKeyset = []
--     theCode = [text|(cancel-transfer $critterid)|]
--     theData = Null
-- createCritterRequest (Nonce nonce) (SetBreeding (CritterId critterid) flag) =
--   mkCommand madeKeyset (def :: PublicMeta) nonce (Exec (ExecMsg theCode theData))
--   where
--     madeKeyset = []
--     theCode = [text|(set-breeding $critterid $flagg)|]
--     theData = Null
--     flagg = if flag
--                then "true"
--                else "false"
-- createCritterRequest (Nonce nonce) (SolicitMate (CritterId critterid)) =
--   mkCommand madeKeyset (def :: PublicMeta) nonce (Exec (ExecMsg theCode theData))
--   where
--     madeKeyset = []
--     theCode = [text|(solicit-mate $critterid)|]
--     theData = Null
-- createCritterRequest (Nonce nonce) (CombineGenes (Genes genesA) (Genes genesB)) =
--   mkCommand madeKeyset (def :: PublicMeta) nonce (Exec (ExecMsg theCode theData))
--   where
--     madeKeyset = []
--     theCode = [text|(combine-genes $genesA $genesB)|]
--     theData = Null
-- createCritterRequest (Nonce nonce) (Breed (CritterId ida) (CritterId idb)) =
--   mkCommand madeKeyset (def :: PublicMeta) nonce (Exec (ExecMsg theCode theData))
--   where
--     madeKeyset = []
--     theCode = [text|(breed $ida $idb)|]
--     theData = Null
-- createCritterRequest (Nonce nonce) (CancelBreed (CritterId critterid)) =
--   mkCommand madeKeyset (def :: PublicMeta) nonce (Exec (ExecMsg theCode theData))
--   where
--     madeKeyset = []
--     theCode = [text|(cancel-breed $critterid)|]
--     theData = Null

mkRandomCritterRequest :: StdGen -> FGen CritterRequest
mkRandomCritterRequest gen = go
  where
    go =
      let (i, _gen') = randomR (0, 13 :: Int) gen
       in case i of
            0 -> GetIncCount <$> undefined
            1 -> CreateCritter <$> undefined
            -- 2 -> ShowCritter <$> undefined <*> undefined
            2 -> ShowGeneration <$> undefined
            3 -> Owner <$> undefined
            4 -> TransferCritter <$> undefined <*> undefined
            5 -> SetTransfer <$> undefined <*> undefined <*> undefined
            6 -> InitiateTransfer <$> undefined <*> undefined
            7 -> CompleteTransfer <$> undefined
            8 -> CancelTransfer <$> undefined
            9 -> SetBreeding <$> undefined <*> undefined
            10 -> SolicitMate <$> undefined
            11 -> CombineGenes <$> undefined <*> undefined
            12 -> Breed <$> undefined <*> undefined
            13 -> CancelBreed <$> undefined
            _ -> fail "mkRandomCritterRequest: You should not hit the case."

data CryptoCritter = CryptoCritter
  { cgenes :: Text
  , cmatronid :: Integer
  , csireid :: Integer
  , cgeneration :: Integer
  , cowner :: [SomeKeyPair]
  , ctransferring :: Bool
  , ctransferto :: [SomeKeyPair]
  , cavailabletobreed :: Bool
  }

-- -- interpolateCritterJSONObject :: Parser CryptoCritter
-- interpolateCritterJSONObject critter = do
--   genes        <- critter .: "genes"
--   matronid     <- critter .: "matron-id"
--   sireid       <- critter .: "sire-id"
--   generation   <- critter .: "generation"
--   owner        <- critter .: "owner"
--   transferring <- critter .: "transferring"
--   transferto   <- critter .: "transfer-to"
--   availableto  <- critter .: "availble-to"
--   return $
--     CryptoCritter
--     <$> genes
--     <*> matronid
--     <*> sireid
--     <*> generation
--     <*> owner
--     <*> transferring
--     <*> transferto
--     <*> availableto
