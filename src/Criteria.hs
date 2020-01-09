{-| Functions that specify properties that need to hold between users.



-}

module Criteria
  ( -- * Criteria functions.
    watchedCriteria
  , timeWatchedCriteria
  , animeCountCriteria
    -- * Useful getter functions.
  , timeWatched
  , animeCount
  ) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Query.Datatypes

-- | Determines whether the user has watched at least as many anime as
-- pAuthUser as well as watched at least as many minutes of anime.
-- Combines timeWatchedCriteria and animeCountCriteria.
watchedCriteria :: User -> User -> Bool
watchedCriteria pAuth user = timeWatchedCriteria pAuth user &&
  animeCountCriteria pAuth user

-- | The user has to have at least as many minutes watched as pAuthUser.
timeWatchedCriteria :: User -> User -> Bool
timeWatchedCriteria pAuth user = timeWatched user >= timeWatched pAuth

-- | The user has to have at least as much anime watched as pAuth.
animeCountCriteria :: User -> User -> Bool
animeCountCriteria pAuth user = animeCount user >= animeCount pAuth

-- | Getter function for userStatisticsMinutesWatched which gets rid of the
-- Maybe wrapper.
timeWatched :: User -> Int
timeWatched = fromJust . userStatisticsMinutesWatched . fromJust .
  userStatisticTypesAnime . fromJust . userStatistics

-- | Getter function for userStatisticsCount which gets rid of the Maybe
-- wrapper.
animeCount :: User -> Int
animeCount = fromJust . userStatisticsCount . fromJust .
  userStatisticTypesAnime . fromJust . userStatistics