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
import Query.Request (request)
import Query.Response
import Query.Service (servicePsuedoAuthUser)
import System.IO (stderr, hPrint)

spec :: Spec
spec = do
  requestSpec

runRequest :: IO Value
runRequest = do
  service <- servicePsuedoAuthUser "Drens5"
  runReq defaultHttpConfig $ request service

runRequestWithPrint :: IO Value
runRequestWithPrint = do
  service <- servicePsuedoAuthUser "Drens5"
  v <- runReq defaultHttpConfig $ request service
  hPrint stderr v
  return v

-- | Tests for request function.
-- Checks one field to see if the request succeeded
requestSpec :: Spec
requestSpec = before runRequest $ do
  describe "requestSpec" $ do
    it "has a non-null data field" $ \r -> do
      ((isJust . psuedoAuthUser) <$> dataPsuedoAuthUser r) `shouldBe` Success True