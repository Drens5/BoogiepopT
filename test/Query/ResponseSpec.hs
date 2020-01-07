{-# LANGUAGE OverloadedStrings #-}

{-| Test module for Response module.

Tests parsing of the datatypes from JSON.

-}

module Query.ResponseSpec
  ( -- * Top level spec function.
    spec
  ) where

import Prelude hiding (readFile)
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString, readFile)
import Test.Hspec
import Query.Datatypes
import Query.Response

spec :: Spec
spec = do
  smallParseTest

smallParseTestCheck :: Result DataPsuedoAuthUser
smallParseTestCheck = Success DataPsuedoAuthUser
  { psuedoAuthUser = smallParseTestUserCheck
  }

smallParseTestUserCheck :: Maybe User
smallParseTestUserCheck = Just User
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

-- | From the top level object in a possible response of a query parse the
-- data field and return it's associated record field type.
decodeToSmallParseTestCheck :: ByteString -> Result DataPsuedoAuthUser
decodeToSmallParseTestCheck toParse = dataPsuedoAuthUser $
  (fromJust . decode) toParse

readTestFileSmallParseTest :: IO ByteString
readTestFileSmallParseTest = readFile "test/Query/Responses/SmallParseTest.json"

-- | Tests parsing of a small possible response from a GraphQL request.
smallParseTest :: Spec
smallParseTest = before readTestFileSmallParseTest $ do
  describe "small FromJSON instances parse test" $ do
    it "allows for simple parsing from JSON" $ \json -> do
      decodeToSmallParseTestCheck json `shouldBe` smallParseTestCheck