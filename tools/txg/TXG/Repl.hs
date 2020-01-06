-- | Porcelain for the transaction generator in the REPL. Many of these
-- functions are very unsafe because they are designed for maximum convenience
-- in ghci.  Do not depend on this module from important code!

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TXG.Repl
  (
  -- * Core functions
    send
  , poll
  , local
  , cmd

  -- * Specific kinds of transactions
  , transfer
  , transferCreate

  -- * Various convenience functions
  , rk
  , chain
  , chain0
  , genTestModules
  , host
  , verToChainId
  , verToChainIdMin
  , listenResponse
  , mkCmdStr
  , mkCmdDataStr
  , mkKey
  , mkKeyCombined
  , k2g
  , mkGuard
  , mkGuardCombined
  , pollResponse
  , randomCmd
  , stockKey
  , mkKeyset
  , signedCode
  , testAdminKey
  , verToPactNetId

  , module Chainweb.ChainId
  , module Chainweb.Version
  , module Pact.Types.ChainMeta
  , module TXG.Simulate.Contracts.CoinContract
  ) where

import Control.Exception
import Control.Lens hiding ((.=), from, to)
import Control.Monad (forM)

import Data.Aeson.Types (Parser)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Random
import Data.Decimal
import Data.Foldable
import Data.HashSet ()
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import Data.Ratio
import Data.String
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time.Clock.POSIX


import Servant.Client

import Test.RandomStrings
import Text.Printf

import Pact.ApiReq
import Pact.GasModel.Utils
import Pact.Parse
import Pact.Types.API
import qualified Pact.Types.ChainId as P
import Pact.Types.ChainMeta
import Pact.Types.Command
import Pact.Types.Crypto
import Pact.Types.Gas
-- import Pact.Types.Hash
import Pact.Types.Scheme
import Pact.Types.Util

import Chainweb.ChainId
import Chainweb.HostAddress
import Chainweb.Version

import System.Time.Extra

import TXG.ReplInternals
import TXG.Simulate.Contracts.CoinContract
import TXG.Simulate.Contracts.Common
import TXG.Simulate.Utils

-- Helper for simplifying construction of RequestKeys
rk :: String -> RequestKeys
rk s = RequestKeys $ fromString s :| []

host :: String -> HostAddress
host h = HostAddress hn 443
  where
    Just hn = hostnameFromText $ T.pack h

instance IsString RequestKey where
  fromString s = RequestKey h
    where
      Right h = fromText' (T.pack s)

-- | Easy way to construct a ChainId
chain :: Int -> ChainId
chain n = fromJust $ chainIdFromText $ T.pack $ show n

-- | ChainId for the most commonly used chain
chain0 :: ChainId
chain0 = fromJust $ chainIdFromText "0"

-- | Decodes a base16-encoded key into a ByteString
mkKeyBS :: Text -> ByteString
mkKeyBS = decodeKey . encodeUtf8

mkKey :: Text -> Text -> SomeKeyPair
mkKey pub priv = skp
  where
    Right skp = importKeyPair defaultScheme (Just $ PubBS $ mkKeyBS pub) (PrivBS $ mkKeyBS priv)

-- | Pact-web's private key copy/paste feature copies a string that contains the
-- private and public keys concatenated together. This function makes it easy to
-- make key pairs from those strings.
mkKeyCombined :: Text -> SomeKeyPair
mkKeyCombined pactWebPriv = mkKey pub priv
  where
    (priv,pub) = T.splitAt (T.length pactWebPriv `div` 2) pactWebPriv

k2g :: SomeKeyPair -> Guard
k2g skp = Guard $ pure (skp, [])

mkGuard pub priv = k2g $ mkKey pub priv
mkGuardCombined pactWebPriv = k2g $ mkKeyCombined pactWebPriv

signedCode
  :: [SomeKeyPairCaps]
  -- ^ Key pair to sign with
  -> String
  -- ^ Pact code
  -> IO [Command Text]
signedCode k c =
  fmap (:[]) (txToCommand defChainwebVersion defPubMeta (NEL.fromList k) (PactCode c))

-- | Convenience function for constructing a coin transfer transaction
transfer :: Text -> Text -> Double -> IO [Command Text]
transfer from to amt = do
    k <- stockKey from
    let meta = defPubMeta { _pmSender = from }
    kps <- mkKeyPairs [k]
    fmap (:[]) $ txToCommand defChainwebVersion meta (NEL.fromList kps) $
      CallBuiltin $ CC $ CoinTransfer
        (SenderName $ Account $ T.unpack from)
        (ReceiverName $ Account $ T.unpack to)
        (Amount $ realFracToDecimal 12 amt)

-- | Convenience function for constructing a transfer-and-create transaction
transferCreate :: Text -> Text -> Guard -> Double -> IO [Command Text]
transferCreate from to guard amt = do
  k <- stockKey from
  let meta = defPubMeta { _pmSender = from }
  kps <- mkKeyPairs [k]
  fmap (:[]) $ txToCommand defChainwebVersion meta (NEL.fromList kps) $
    CallBuiltin $ CC $ CoinTransferAndCreate
      (SenderName $ Account $ T.unpack from)
      (ReceiverName $ Account $ T.unpack to)
      guard
      (Amount $ realFracToDecimal 12 amt)

mkKeyset :: Text -> [PublicKeyBS] -> Value
mkKeyset p ks = object
  [ "pred" .= p
  , "keys" .= ks
  ]

sampleKeyPairCaps :: IO [SomeKeyPairCaps]
sampleKeyPairCaps = do
  s <- stockKey "sender00"
  mkKeyPairs [s]

mkCmdStr :: PublicMeta -> ChainwebVersion -> [SomeKeyPairCaps] -> String -> IO (Command Text)
mkCmdStr meta ver kps str =
  cmd str Null meta kps (Just (verToPactNetId ver)) Nothing

mkCmdDataStr
  :: PublicMeta
  -> ChainwebVersion
  -> [SomeKeyPairCaps]
  -> String
  -> Value
  -> IO (Command Text)
mkCmdDataStr meta ver kps cmdCode cmdData =
  cmd cmdCode cmdData meta kps (Just (verToPactNetId ver)) Nothing

verToChainId :: ChainwebVersion -> ChainId
verToChainId ver = foldr const err $ chainIds ver
  where
    err = error "Chainweb version has 0 chains"

-- get the minimum ChainId from a Version
verToChainIdMin :: ChainwebVersion -> ChainId
verToChainIdMin ver =
  foldr min z0 ids
  where
    z0 = verToChainId ver -- error on empty set or pick a default for the next foldr
    ids = chainIds ver

verToPactNetId :: ChainwebVersion -> P.NetworkId
verToPactNetId cvw =
  P.NetworkId $ T.pack $ show cvw

pollResponse :: Network -> Either ClientError RequestKeys -> IO ()
pollResponse _nw (Left err) = putStrLn $ "There was a failure in the send: " ++ show err
pollResponse nw (Right rks) = do
  pollResp <- poll nw rks
  case pollResp of
    Left err -> putStrLn $ "Poll error: " ++ show err
    Right pollResponses -> print pollResponses

listenResponse :: Network -> Either ClientError RequestKeys -> IO ()
listenResponse _nw (Left err) = putStrLn $ "There was a failure in the send: " ++ show err
listenResponse nw (Right (RequestKeys (k :| []))) = do
  listResp <- listen nw k
  case listResp of
    Left err -> putStrLn $ "Listen error: " ++ show err
    Right listenResp -> print listenResp
-- listenResponse nw (Right (RequestKeys (rk :| rks))) =
listenResponse _nw (Right _r) =
  putStrLn "Listen can only be used with a single request key"

randomCmd :: PublicMeta -> ChainwebVersion -> IO (Command Text)
randomCmd meta ver = do
  -- 100 strings, w/ lengths 10 to 30, 34 alphabetical and 18 of those upper-case.
  rands <- randomStringsLen (randomString' randomASCII (3%4) (1%8)) (10, 30) 100
  let someWords = "(" ++ unwords rands ++ ")"
  kps <- sampleKeyPairCaps
  mkCmdStr meta ver kps someWords

genTestModules :: PublicMeta -> ChainwebVersion -> Int -> Bool -> IO [Command Text]
genTestModules meta ver nModules createTable = do
  let ints = [0..(nModules-1)]
  forM ints ( \n -> do
    s <- testModule n createTable
    kps <- sampleKeyPairCaps
    mkCmdDataStr meta ver kps s testAdminKey )

testModule :: Int -> Bool -> IO String
testModule n createTable = do
  t <- getPOSIXTime
  let z = round t :: Integer
  let theKey = show z ++ show n
  let s = "(define-keyset 'module-admin-" ++ theKey ++ " (read-keyset \"test-module-keyset\"))"
          ++ "\n" ++ "(namespace 'free)"
          ++ "\n" ++ "(module test-module-" ++ theKey ++ " 'module-admin-" ++ theKey
          ++ "\n" ++ "(defschema test-module-schema-" ++ theKey
          ++ "\n" ++ "accountId:string"
          ++ "\n" ++ "name:string"
          ++ "\n" ++ "balance:decimal)"
          ++ "\n" ++ "(deftable test-module-tbl-" ++ theKey
          ++ ":{test-module-schema-" ++ theKey ++ "})"
          ++ "\n" ++ "(defun insert-row (id name bal)"
          ++ "\n" ++ "(insert test-module-tbl-" ++ theKey ++ " id"
          ++ "\n" ++ "{ \"accountId\": id, \"name\": name, \"balance\": bal }))"
          ++ "\n" ++ ")"
  return $ if createTable
             then s ++ testTableCreate n theKey
             else s

testTableCreate :: Int -> String -> String
testTableCreate n theKey =
  "\n" ++ "(create-table test-module-tbl-" ++ theKey ++ ")"
  ++ "\n" ++ "(insert-row (id-" ++ theKey ++ " name-" ++ show n ++ " " ++ show n ++ ")"

testAdminKey :: Value
testAdminKey =
  object [ "test-module-keyset" .= kSet ]
  where
    kSet = object
      [ "pred" .= p
      , "keys" .= ks
      ]
    p = ">" :: Text
    ks = [PubBS "sender00"] :: [PublicKeyBS]

-- **************************************************
-- The following are all temporarily very useful to avoid entering many repetitive REPL commands.
-- These can eventually be deleted
-- **************************************************
_testAdminKeySet :: Value
_testAdminKeySet =
  let t = "{\"data\": {\"test-module-keyset\": {\"keys\":[\"sender00\"], \"pred\":\"<\"}}}" :: Text
  in toJSON t

_hostAddr :: HostAddress
_hostAddr = host "us1.tn1.chainweb.com"

_ver :: ChainwebVersion
_ver = Development

_cid :: ChainId
_cid = verToChainIdMin _ver

_nw :: Network
_nw = Network _ver _hostAddr _cid

_metaIO :: IO PublicMeta
_metaIO = makeMeta _cid defTTL defGasPrice defGasLimit

_metaIO' :: GasLimit -> IO PublicMeta
_metaIO' = makeMeta _cid defTTL defGasPrice

_cmd1IO :: IO (Command Text)
_cmd1IO = do
  meta <- _metaIO
  kps <- sampleKeyPairCaps
  mkCmdStr meta _ver kps "(+ 1 1)"

_cmd2IO :: IO (Command Text)
_cmd2IO = do
  meta <- _metaIO
  kps <- sampleKeyPairCaps
  mkCmdStr meta _ver kps "(+ 2 2)"

_cmdSIO :: String -> IO (Command Text)
_cmdSIO s = do
  meta <- _metaIO
  kps <- sampleKeyPairCaps
  mkCmdStr meta _ver kps s

_cmdSIO' :: GasLimit -> String -> IO (Command Text)
_cmdSIO' gl s = do
  meta <- _metaIO' gl
  kps <- sampleKeyPairCaps
  mkCmdStr meta _ver kps s

_sendIt :: Command Text -> IO (Either ClientError RequestKeys)
_sendIt theCmd = _sendThem [theCmd]

_sendlm :: IO (Either ClientError RequestKeys)
_sendlm = do
  theCmd <- _cmdSIO "(list-modules)"
  _sendIt theCmd

_sendThem :: [Command Text] -> IO (Either ClientError RequestKeys)
_sendThem theCmds = send _nw theCmds

_tblRows :: IO (Either ClientError RequestKeys)
_tblRows = do
  theCmd <- _cmdSIO "(free.csv-import.table-len)"
  _sendIt theCmd

_tblRows' :: GasLimit -> IO (Either ClientError RequestKeys)
_tblRows' gl = do
  theCmd <- _cmdSIO' gl "(free.csv-import.table-len)"
  _sendIt theCmd

_testMods :: Int -> Bool -> IO (Either ClientError RequestKeys)
_testMods nModules createTable = do
  meta <- _metaIO
  cmds <- genTestModules meta _ver nModules createTable
  _sendThem cmds
