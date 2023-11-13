{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Test.Pact.SPV.Hyperlane
( -- * test suite
  tests
) where

import Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as Binary
import Data.Text (Text)

import Test.Tasty
import Test.Tasty.HUnit

-- internal pact modules

import Pact.Types.Exp
import Pact.Types.Term
import Pact.Types.Util (AsString(..))

-- internal chainweb modules

import Chainweb.Pact.SPV.Hyperlane
import Chainweb.Pact.SPV.Hyperlane.Binary

tests :: TestTree
tests = testGroup "hyperlane"
  [ testCase "empty object" hyperlaneEmptyObject

  , testCase "encodeTokenMessageERC20" hyperlaneEncodeTokenMessageERC20
  , testCase "decodeTokenMessageERC20" hyperlaneDecodeTokenMessageERC20
  , testCase "wordToDecimal" hyperlaneWordToDecimal

  , testCase "encodeHyperlaneMessage" hyperlaneEncodeHyperlaneMessage
  ]

hyperlaneEmptyObject :: Assertion
hyperlaneEmptyObject = do
  res <- runExceptT $ evalHyperlaneCommand $ mkObject []
  assertEqual "should fail with missing command name" (Left "Unknown hyperlane command") res

hyperlaneEncodeTokenMessageERC20 :: Assertion
hyperlaneEncodeTokenMessageERC20 = do
  let
    res = encodeTokenMessageERC20 $ mkObject
        [ ("recipient", tStr $ asString ("recipient" :: Text))
          , ("amount", tLit $ LDecimal 3333333333333333333.333333333333333333) ]
  case res of
    Nothing -> assertFailure "Should get the result"
    Just t ->
      let
        expectedMessage :: Text = "0x0000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000281fa059c9e179daafc1235555555550000000000000000000000000000000000000000000000000000000000000009726563697069656e740000000000000000000000000000000000000000000000"
      in assertEqual "should get encoded message" expectedMessage t

hyperlaneDecodeTokenMessageERC20 :: Assertion
hyperlaneDecodeTokenMessageERC20 = do
  let
    encodedMessage :: Text = "0x0000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000281fa059c9e179daafc1235555555550000000000000000000000000000000000000000000000000000000000000009726563697069656e740000000000000000000000000000000000000000000000"
  encodedBinary <- case decodeHex encodedMessage of
      Right r -> pure r
      Left _ -> assertFailure "hyperlaneDecodeTokenMessageERC20: failed to decode"
  let
    tm = Binary.decode $ BL.fromStrict encodedBinary
    expectedObject = TokenMessageERC20 "recipient" 3333333333333333333333333333333333333
  assertEqual "Should properly decode the object" expectedObject tm

hyperlaneWordToDecimal :: Assertion
hyperlaneWordToDecimal = assertEqual "" 3333333333333333333.333333333333333333 (wordToDecimal 3333333333333333333333333333333333333)

hyperlaneEncodeHyperlaneMessage :: Assertion
hyperlaneEncodeHyperlaneMessage = do
  let
    obj' = mkObject
        [ ("message", obj
          [ ("version", tLit $ LInteger 1)
          , ("nonce", tLit $ LInteger 1223)
          , ("originDomain", tLit $ LInteger 626)
          , ("sender", tStr $ asString ("0x23" :: Text))
          , ("destinationDomain", tLit $ LInteger 8)
          , ("recipient", tStr $ asString ("0x71c7656ec7ab88b098defb751b7401b5f6d8976f" :: Text))
          , ("tokenMessage", obj
              [ ("recipient", tStr $ asString ("recipient1" :: Text))
              , ("amount", tLit $ LDecimal 3333333333333333333.333333333333333333) ]
            )
          ])
        ]
  res <- runExceptT $ evalHyperlaneCommand obj'
  case res of
    Left err -> assertFailure $ "Should get the result" ++ show err
    Right o ->
      let
        expectedMessage :: Text = "0x01000004c70000027200000000000000000000000000000000000000000000000000000000000000230000000800000000000000000000000071c7656ec7ab88b098defb751b7401b5f6d8976f0000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000281fa059c9e179daafc123555555555000000000000000000000000000000000000000000000000000000000000000a726563697069656e743100000000000000000000000000000000000000000000"
        expectedObject = mkObject
          [ ("encodedMessage", tStr $ asString expectedMessage)
          , ("messageId", tStr $ asString ("0xd7d22abdd68430821df0732b7bb87cd05536810cc9fc29d3fbe22cfd359ba190" :: Text))
          ]
      in assertEqual "should get encoded message" expectedObject o