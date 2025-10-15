{-# language ImportQualifiedPost #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# language LambdaCase #-}

module Chainweb.Test.ResponseSizeLimiter (tests) where

import Chainweb.Chainweb.PeerResources qualified as PeerResources
import Chainweb.Graph
import Chainweb.Test.TestVersions
import Chainweb.Test.Utils
import Chainweb.Utils (ResponseBodyTooLarge(..))
import Control.Exception (fromException)
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.ByteString.Lazy.Char8 qualified as ByteString.Lazy.Char8
import Data.Char
import Data.Functor (void)
import P2P.Node.PeerDB qualified as PeerDB
import Network.Wai qualified as Wai
import Network.Wai.Middleware.Gzip qualified as Wai.Middleware.Gzip
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Test.Tasty
import Test.Tasty.HUnit
import PropertyMatchers ((?))
import PropertyMatchers qualified as P


tests :: TestTree
tests = testCase "Chainweb.Test.ResponseSizeLimiter" $ do
    (mgr, _) <- PeerResources.connectionManager =<< PeerDB.newEmptyPeerDb (timedConsensusVersion singletonChainGraph pairChainGraph)

    runResourceT $ do
        let smallString = "small string"
        req <- requestForServer id smallString
        liftIO $ void $ HTTP.httpLbs req mgr

    runResourceT $ do
        let limitString =
                ByteString.Lazy.Char8.replicate (2 * 1024 * 1024) 'a'
        req <- requestForServer id limitString
        liftIO $ void $ HTTP.httpLbs req mgr

    do
        let excessiveString =
                ByteString.Lazy.Char8.replicate (2 * 1024 * 1024 + 1) 'a'
        runResourceT $ do
            req <- requestForServer id excessiveString
            liftIO $ do
                HTTP.withResponse req mgr $ \resp -> do
                    -- we would like to be able to read *most* of the response before we
                    -- throw an error.
                    void $ HTTP.brReadSome (HTTP.responseBody resp) (1 * 1024 * 1024)
                    HTTP.brConsume (HTTP.responseBody resp)
                        & P.fun void
                        ? P.throws
                        ? (\case
                            HTTP.HttpExceptionRequest req' (HTTP.InternalException ex) -> do
                                ex
                                    & P.fun fromException
                                    ? P.equals (Just ResponseBodyTooLarge)
                                req'
                                    & P.fun show
                                    ? P.equals (show req)
                            ex -> P.fail "HTTP.InternalException" ex
                        )

        runResourceT $ do
            let gzipMiddleware = Wai.Middleware.Gzip.gzip Wai.Middleware.Gzip.defaultGzipSettings
            req <- requestForServer gzipMiddleware excessiveString
            liftIO $ do
                HTTP.withResponse req mgr $ \resp -> do
                    -- the gzip middleware doesn't always compress responses,
                    -- but we want to check that it does here.
                    resp
                        & P.fun HTTP.responseHeaders
                        ? P.fun (lookup HTTP.hContentEncoding)
                        ? P.match _Just
                        ? P.equals "gzip"

                    -- we would like to be able to read *most* of the response before we
                    -- throw an error. here we also check what it is, to ensure
                    -- that the gzipped content is being decompressed.
                    HTTP.brReadSome (HTTP.responseBody resp) (1 * 1024 * 1024)
                        >>= P.checkAll
                            [ traverseOf_ each (P.equals (fromIntegral (ord 'a')))
                            , P.fun ByteString.Lazy.length ? P.lt (2 * 1024 * 1024)
                            ]
                    HTTP.brConsume (HTTP.responseBody resp)
                        & P.fun void
                        ? P.throws
                        ? (\case
                            HTTP.HttpExceptionRequest req' (HTTP.InternalException ex) -> do
                                ex
                                    & P.fun fromException
                                    ? P.equals (Just ResponseBodyTooLarge)
                                req'
                                    & P.fun show
                                    ? P.equals (show req)
                            ex -> P.fail "HTTP.InternalException" ex
                        )

    where
    requestForServer :: Wai.Middleware -> ByteString.Lazy.ByteString -> ResourceT IO HTTP.Request
    requestForServer middleware responseString = do
        -- can't use tls here as the cert is self-signed and we want to use the
        -- "real" p2p connection manager.
        usedPort <- withTestAppServerR False $ middleware $ \_req sendResp -> do
            -- content-type must be set or the gzip wai middleware will never
            -- compress the response body.
            sendResp $ Wai.responseLBS HTTP.status200 [("Content-Type", "text/plain")] responseString
        liftIO $ do
            HTTP.parseRequest ("http://localhost:" <> show usedPort)
