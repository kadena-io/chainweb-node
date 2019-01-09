{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Chainweb.Test.Pact
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: See LICENSE file
-- Maintainer: Mark Nichols <mark@kadena.io>
-- Stability: experimental
--
-- Unit test for Pact execution in Chainweb

module Chainweb.Test.Pact where

import Chainweb.Pact.Backend.InMemoryCheckpointer
import Chainweb.Pact.Backend.SQLiteCheckpointer
import Chainweb.Pact.PactService
import Chainweb.Pact.Types

import qualified Pact.Gas as P
import qualified Pact.Interpreter as P
import qualified Pact.Types.API as P
import qualified Pact.Types.Crypto as P
import qualified Pact.Types.Gas as P
import qualified Pact.Types.Logger as P
import qualified Pact.Types.Server as P

import Control.Applicative
import Control.Monad.Trans.RWS.Lazy
import Data.Aeson (Value(..))
import Data.Functor
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.Maybe
import Data.Scientific
import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testCase "Pact unit tests" pactExecTests
    -- [ testGroup "Simple Pact Execution"
        -- [testCase "simpleExec" simpleExec]
    -- ]

pactExecTests :: IO ()
pactExecTests = do
    let loggers = P.neverLog
    let logger = P.newLogger loggers $ P.LogName "PactService"
    pactCfg <- setupConfig "pact.yaml" -- TODO: file name/location from configuration
    let cmdConfig = toCommandConfig pactCfg
    let gasLimit = fromMaybe 0 (P._ccGasLimit cmdConfig)
    let gasRate = fromMaybe 0 (P._ccGasRate cmdConfig)
    let gasEnv = P.GasEnv (fromIntegral gasLimit) 0.0 (P.constGasModel (fromIntegral gasRate))
    (checkpointEnv, theState) <-
        case P._ccSqlite cmdConfig of
            Nothing -> do
                env <- P.mkPureEnv loggers
                liftA2 (,) (initInMemoryCheckpointEnv cmdConfig logger gasEnv)
                    (mkPureState env cmdConfig)
            Just sqlc -> do
                env <- P.mkSQLiteEnv logger False sqlc loggers
                liftA2 (,) (initSQLiteCheckpointEnv cmdConfig logger gasEnv)
                    (mkSQLiteState env cmdConfig)
    void $ runRWST execTests checkpointEnv theState

execTests :: PactT ()
execTests = do
    -- forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
    forM_ testPactRequests (\tr ->

    -- need:
    -- kps: list of P.KeyPair
    -- for the kadena test:
    --     _keys = [KeyPair (_ccSecretKey conf) (_ccPublicKey conf)]
    --         where _ccSecretKey and _ccPublicKey come from:
    --             YAML.decodeFileEither on client.yaml containing:
    --                 publicKey: 201a45a367e5ebc8ca5bba94602419749f452a85b7e9144f29a99f3f906c0dbc
    --                          importPublic :: ByteString -> Maybe PublicKey
    --                        :: P.PublicKey
    --                          -- now use PubKey instead of PublicKey ?
    --                 secretKey: 53108fc90b19a24aa7724184e6b9c6c1d3247765be4535906342bd5f8138f7d2
    --                        :: P.PrivateKey
    --                          importPrivate :: ByteString -> Maybe PublicKey
    -- code: Pact code as string
    -- reqId: request id as Text i.e., (pack . show) on Int64
    -- theData: Null -- :: <some a st. (ToJSON a)>
        Pact.mkCommand
            (map (\Pact.KeyPair {..} -> (Pact.Ed25519, _kpSecret, _kpPublic)) kps)
            Nothing
            reqId
            theData



execCmd :: P.Command ByteString -> TransactionOutput
execCmd cmd = do

{- -- (Kadena's mkExec)
mkExec :: String -> Value -> Maybe Pact.Address -> Repl (Pact.Command T.Text)
mkExec code mdata addy = do
  kps <- use keys
  rid <- use requestId >>= liftIO . (`modifyMVar` (\i -> return $ (succ i, i)))
  return $ decodeUtf8 <$>
    Pact.mkCommand
    (map (\Pact.KeyPair {..} -> (Pact.ED25519,_kpSecret,_kpPublic)) kps)
    addy
    (T.pack $ show rid)
    (Exec (ExecMsg (T.pack code) mdata))
-}


{-
mkCommand :: ToJSON a => [(PPKScheme, PrivateKey, Base.PublicKey)] -> Maybe Address -> Text -> a
                      -> Command ByteString
mkCommand creds addy nonce a = mkCommand' creds $ BSL.toStrict $ A.encode (Payload a nonce addy)

mkCommand' :: [(PPKScheme, PrivateKey, Base.PublicKey)] -> ByteString -> Command ByteString
mkCommand' creds env = Command env (sig <$> creds) hsh
  where
    hsh = hash env
    sig (scheme, sk, pk) = UserSig scheme (toB16Text $ exportPublic pk) (toB16Text $ exportSignature $ sign hsh sk pk)
-}


----------------------------------------------------------------------------------------------------
data TestRequest = TestRequest
    { cmd :: String
    , eval :: TestResponse -> Assertion
    , displayStr :: String
    }

instance Show TestRequest where
    show tr = "cmd: " ++ cmd tr ++ "\nDisplay string: " ++ displayStr tr

data TestResponse = TestResponse
    { resultSuccess :: Bool
    , apiResult :: P.ApiResult
    , _batchCount :: Int64
    } deriving (Eq, Generic)

instance Show TestResponse where
    show tr = "resultSuccess: " ++ show (resultSuccess tr) ++ "\n"
        ++ "Batch count: " ++ show (_batchCount tr) ++ "\n"
        ++ take 100 (show (apiResult tr)) ++ "..."

----------------------------------------------------------------------------------------------------
checkScientific :: Scientific -> TestResponse -> Assertion
checkScientific sci tr = do
  --resultSuccess tr `shouldBe` True
  --parseScientific (P._arResult $ apiResult tr) `shouldBe` Just sci
  resultSuccess tr @? "resultSuccess was not set to True"
  parseScientific (P._arResult $ apiResult tr) @?= Just sci

parseScientific :: Value -> Maybe Scientific
parseScientific (Object o) =
  case HM.lookup "data" o of
    Nothing -> Nothing
    Just (Number sci) -> Just sci
    Just _ -> Nothing
parseScientific _ = Nothing

----------------------------------------------------------------------------------------------------
testPactRequests :: [TestRequest]
testPactRequest = [testReq1]

testReq1 :: TestRequest
testReq1 = TestRequest
    { cmd = "exec (+ 1 1)"
    , eval = (\tr -> checkScientific (scientific 2 0) tr)
    , displayStr = "Executes 1 + 1 in Pact and returns 2.0" }
