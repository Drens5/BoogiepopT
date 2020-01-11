{-# LANGUAGE OverloadedStrings #-}

{-| Test module for CoupledGT module.

Algorithm for finding recommendations for a user.

-}

module Recco.CoupledGTSpec
  ( -- * Top level spec function.
    spec
  ) where

import Condition
import Query.Datatypes
import Recco.CoupledGT
import System.IO (stderr, hPrint)
import Test.Hspec

spec :: Spec
spec = do
  describe "Tests are turned off." $ do
    it "" $ do
      pending
  -- mockTestCoupledGTSpec

testUser :: User
testUser = User
  { userId = Just 137485
  , userName = Nothing
  , userSiteUrl = Just "https://anilist.co/user/137485"
  , userStatistics = smallParseTestUserStatisticsCheck
  }

smallParseTestUserStatisticsCheck :: Maybe UserStatisticTypes
smallParseTestUserStatisticsCheck = Just UserStatisticTypes
  { userStatisticTypesAnime = smallParseTestUserStatisticTypesAnime
  }

smallParseTestUserStatisticTypesAnime :: Maybe UserStatistics
smallParseTestUserStatisticTypesAnime = Just UserStatistics
  { userStatisticsCount = Just 302
  , userStatisticsMinutesWatched = Just 182457
  , userStatisticsGenres = Nothing
  , userStatisticsTags = Nothing
  }

runCoupledGTSpecWithPrint :: IO [(Int, Media)]
runCoupledGTSpecWithPrint = do
  v <- coupledGT testUser [Strong "Comedy", Weak ["Slice of Life", "Drama",
    "Adventure"]] [Weak ["Surreal Comedy", "Slapstick", "Satire", "Parody"]]
      10 3
  hPrint stderr v
  return v

-- | Do a test run and see what it brings on stderr.
mockTestCoupledGTSpec :: Spec
mockTestCoupledGTSpec = before runCoupledGTSpecWithPrint $ do
  describe "mockTestCoupledGTSpec" $ do
    it "shows up on stderr, but is also an non-empty list" $ \res -> do
      not (null res) `shouldBe` True