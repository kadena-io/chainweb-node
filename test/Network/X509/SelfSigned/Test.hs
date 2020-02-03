{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Network.X509.SelfSigned.Test
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Kadena Chainweb Team <chainweb-dev@kadena.io>
-- Stability: experimental
--
-- TODO
--
module Network.X509.SelfSigned.Test
( tests
) where

import Control.Applicative
import Control.Concurrent.Async (withAsync)
import Control.Exception
import Control.Monad hiding (fail)

import qualified Data.ByteString.Char8 as B8 (pack)

import Network.HTTP.Client hiding (port)
import Network.HTTP.Types (status200, statusIsSuccessful)
import Network.Socket (Socket, close)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, openFreePort)
import Network.Wai.Handler.WarpTLS as WARP (runTLSSocket)

import Prelude hiding (fail)

import System.Environment

import Test.Tasty
import Test.Tasty.HUnit

-- internal modules

import Network.X509.SelfSigned

-- -------------------------------------------------------------------------- --
-- Tests

tests :: TestTree
tests = testGroup "SelfSignedCertificate Tests"
    [ testCertType @RsaCert "RSA"
    -- Disabling because of failures on Linux CI and not critically needed
    -- , testCertType @Ed25519Cert "Ed25519"
    -- , testCertType @Ed448Cert "Ed448"
    -- , testCertType @P256Cert "P-256"
        -- P-256 is currently not fully supported on the server side
        -- in the master branch of the tls package.
    ]

testCertType :: forall k . X509Key k => String -> TestTree
testCertType l = testCaseSteps l $ \step -> do

    step "Generate Certificate"

    (fp, cert, key) <- generateLocalhostCertificate @k 1
    let cred = unsafeMakeCredential (X509CertChainPem cert []) key

    step "Start Server"
    bracket openFreePort (close . snd) $ \(p, sock) -> do
        withAsync (serve cert key sock) $ \_ -> do

            let bp = B8.pack $ show p

            -- Certificate Validation Cache
            let check ("localhost", x) | x == bp = return $ Just fp
                check ("127.0.0.1", x) | x == bp = return $ Just fp
                check _ = return Nothing

            -- Test Query
            let query h (port :: Int) policy = do
                    let uri = "https://" <> h <> ":" <> show port
                    step $ "query " <> uri <> " with " <> showPolicy policy
                    mgr <- newManager =<< certificateCacheManagerSettings policy (Just cred)
                    req <- parseRequest uri
                    rsp <- httpLbs req mgr
                    return (statusIsSuccessful (responseStatus rsp), rsp)

            -- Test Functions
            let pass a = a >>= \(s,r) -> assertBool (show r) s
                fail = assertException @HttpException

            step "start tests"

            pass $ query "localhost" p TlsInsecure
            pass $ query "localhost" p $ TlsSecure True check
            pass $ query "localhost" p $ TlsSecure False check
            fail $ query "localhost" p $ TlsSecure True (\_ -> return Nothing)
            fail $ query "localhost" p $ TlsSecure False (\_ -> return Nothing)

            pass $ query "127.0.0.1" p TlsInsecure
            pass $ query "127.0.0.1" p $ TlsSecure True check
            pass $ query "127.0.0.1" p $ TlsSecure False check

            -- skip trying to contact google.com when networking is sandboxed
            sbox <- isSandboxed
            unless sbox $ do
                pass $ query "google.com" 443 TlsInsecure
                pass $ query "google.com" 443 $ TlsSecure True check
                fail $ query "google.com" 443 $ TlsSecure False check

  where
    isSandboxed = do
        let isSet m = case m of
                          Nothing -> False
                          Just "0" -> False
                          _ -> True
        m1 <- isSet <$> lookupEnv "SANDBOXED"
        m2 <- isSet <$> lookupEnv "NIX_ENFORCE_PURITY"
        return $! m1 || m2

-- -------------------------------------------------------------------------- --
-- Utils

serve :: X509CertPem -> X509KeyPem -> Socket -> IO ()
serve certBytes keyBytes sock =
    runTLSSocket (tlsServerSettings certBytes keyBytes) defaultSettings sock
        $ \_ respond -> respond $ responseLBS status200 [] "Success"

showPolicy :: TlsPolicy -> String
showPolicy TlsInsecure = "TlsInsecure"
showPolicy (TlsSecure a _) = "TlsInsecure " <> show a <> " <<cache callback>>"

assertException :: forall e a . Exception e => IO a -> Assertion
assertException a = (a >> assertFailure "missing asserted exception")
    `catch` \(_ :: e) -> return ()

-- TODO client certificates:
--     serverHooks = def
--         { onUnverifiedClientCert = return True
--         }
