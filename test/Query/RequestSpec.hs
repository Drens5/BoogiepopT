{-# LANGUAGE OverloadedStrings #-}

{-| Test module to Request module

Tests a GraphQL service request being sent, and the response being parsed.

-}

module Query.RequestSpec
  ( -- * Top level spec function.
    spec
  ) where

import Data.Aeson (Result (..), Value)
import Data.Maybe (isJust)
import Network.HTTP.Req
import Test.Hspec
import Query.Datatypes
import Query.Request (request, runRequest, runRequestSafe)
import Query.Response
import Query.Service
import System.IO (stderr, hPrint)
import Control.Monad (replicateM)

spec :: Spec
spec = do
  describe "Tests are turned off." $ do
    it "" $ do
      pending
  -- requestSpec
  -- mockQueryTestUserMediaList
  -- mockQueryTestArbitraryUsers
  -- requestSafeSpec
  -- rateTest

runRequestWithPrint :: IO Service -> IO Value
runRequestWithPrint service' = do
  service <- service'
  v <- runReq defaultHttpConfig $ request service
  hPrint stderr v
  return v

-- | Tests for request function.
-- Checks one field to see if the request succeeded
requestSpec :: Spec
requestSpec = before (runRequest (servicePsuedoAuthUser "Drens5")) $ do
  describe "requestSpec" $ do
    it "has a non-null data field" $ \r -> do
      ((isJust . psuedoAuthUser) <$> dataPsuedoAuthUser r) `shouldBe` Success True

-- | Tests for runRequestSafe.
-- Succeeds if the data field is non-null
requestSafeSpec :: Spec
requestSafeSpec = before (runRequestSafe (servicePsuedoAuthUser "Drens5")) $ do
  describe "requestSafeSpec" $ do
    it "has a non-null data field" $ \r -> do
      ((isJust . psuedoAuthUser) <$> dataPsuedoAuthUser r) `shouldBe` Success True

-- | Mock tests to see on stderr if we get a reasonable response.
-- Otherwise run with runRequest instead of runRequestWithPrint for check on
-- non-null data field without printing to stderr.
-- Succeeds if the data field is non-null.
mockQueryTestUserMediaList :: Spec
mockQueryTestUserMediaList = before (runRequest (serviceUserMediaList 1 137485)) $ do
  describe "mockQueryTestUserMediaList" $ do
    it "has a non-null data field" $ \r -> do
      ((isJust . userMediaList) <$> dataUserMediaList r) `shouldBe` Success True

ninetyRequests :: IO Value
ninetyRequests = head <$> replicateM 100 (runRequestSafe (serviceArbitraryUsers 1))

mockQueryTestArbitraryUsers :: Spec
mockQueryTestArbitraryUsers = before (runRequest (serviceArbitraryUsers 1)) $ do
  describe "mockQueryTestArbitraryUsers" $ do
    it "has a non-null data field" $ \r -> do
      ((isJust . arbitraryUsers) <$> dataArbitraryUsers r) `shouldBe` Success True

rateTest :: Spec
rateTest = before ninetyRequests $ do
  describe "rateTest" $ do
    it "has a non-null data field" $ \r -> do
      ((isJust . arbitraryUsers) <$> dataArbitraryUsers r) `shouldBe` Success True