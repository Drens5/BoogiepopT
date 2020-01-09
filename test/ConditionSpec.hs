{-# LANGUAGE OverloadedStrings #-}

{-| Test module for Condition module.

Test the conditions defined.

-}

module ConditionSpec
  ( -- * Top level spec function.
    spec
  ) where

import Condition
import Data.Text (Text)
import Query.Datatypes
import Test.Hspec

spec :: Spec
spec = do
  tagConditionSpec
  genreConditionSpec

testMedia :: Media
testMedia = Media
  { mediaId = Just 41
  , mediaTitle = Nothing
  , mediaFormat = Just TV
  , mediaStatus = Nothing
  , mediaDescription = Just "Test Description"
  , mediaEpisodes = Nothing
  , mediaTrailer = Nothing
  , mediaGenres = Just ["Comedy", "Action", "Adventure", "Mystery"]
  , mediaAverageScore = Just 50
  , mediaMeanScore = Just 49
  , mediaPopularity = Nothing
  , mediaTags = Just testMediaTags
  , mediaStats = Nothing
  }

testMediaTags :: [MediaTag]
testMediaTags = [
    MediaTag
  { mediaTagId = Just 1
  , mediaTagName = Just "TestTag 1"
  , mediaTagDescription = Nothing
  , mediaTagCategory = Nothing
  , mediaTagRank = Nothing
  , mediaTagIsMediaSpoiler = Nothing
  }
  , MediaTag
  { mediaTagId = Just 2
  , mediaTagName = Just "TestTag 2"
  , mediaTagDescription = Nothing
  , mediaTagCategory = Nothing
  , mediaTagRank = Nothing
  , mediaTagIsMediaSpoiler = Nothing
  }
  , MediaTag
  { mediaTagId = Just 3
  , mediaTagName = Just "TestTag 3"
  , mediaTagDescription = Nothing
  , mediaTagCategory = Nothing
  , mediaTagRank = Nothing
  , mediaTagIsMediaSpoiler = Nothing
  }
  , MediaTag
  { mediaTagId = Just 4
  , mediaTagName = Just "TestTag 4"
  , mediaTagDescription = Nothing
  , mediaTagCategory = Nothing
  , mediaTagRank = Nothing
  , mediaTagIsMediaSpoiler = Nothing
  }
  , MediaTag
  { mediaTagId = Just 5
  , mediaTagName = Just "TestTag 5"
  , mediaTagDescription = Nothing
  , mediaTagCategory = Nothing
  , mediaTagRank = Nothing
  , mediaTagIsMediaSpoiler = Nothing
  }
  ]

tagConditionSpec :: Spec
tagConditionSpec = do
  describe "tagConditionSpec" $ do
    it "works for strong coupled tags" $ do
      tagCondition testMedia (Strong "TestTag 4") `shouldBe` True
      tagCondition testMedia (Strong "TestTag 6") `shouldBe` False

    it "works for weakly coupled tags" $ do
      tagCondition testMedia (Weak ["TestTag 33", "aasdf", "TestTag 3"])
        `shouldBe` True
      tagCondition testMedia (Weak ["TestTag 33", "aasdf", "TestTag s3s"])
        `shouldBe` False
      tagCondition testMedia (Weak ["TestTag 5", "aasdf", "TestTag 43"])
        `shouldBe` True
      tagCondition testMedia (Weak ["TestTag 5", "aasdf", "TestTag 4"])
        `shouldBe` True
      tagCondition testMedia (Weak ["TestTag 53", "aasdf", "TestTag 4", "as",
        "daffas", "sdffww"]) `shouldBe` True

genreConditionSpec :: Spec
genreConditionSpec = do
  describe "genreConditionSpec" $ do
    it "works for strong coupled genres" $ do
      genreCondition testMedia (Strong "Comedy") `shouldBe` True
      genreCondition testMedia (Strong "Fantasy") `shouldBe` False

    it "works for weak coupled genres" $ do
      genreCondition testMedia (Weak ["sfdasf", "dfaf", "Comedy", "adsfa"])
        `shouldBe` True
      genreCondition testMedia (Weak ["sfdasf", "dfaf", "adfas", "adsfa"])
        `shouldBe` False