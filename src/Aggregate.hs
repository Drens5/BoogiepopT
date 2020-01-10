{-| Aggregating list of media to recommendation.



-}

module Aggregate
  ( -- * Organize lists of media.
    nubC
  , mostOccurring
  , nubD
  , highestRated
  ) where

import Data.List (sortOn)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Query.Datatypes

-- | Folds the list of media to a nubbed version where the amount of occurrences
-- is recorded.
nubC :: [Media] -> [(Int, Media)]
nubC medias = foldr step [] medias
  where
    step media acc
      -- The media has already been counted, do nothing.
      | (fromJust . mediaId) media `elem` map (fromJust . mediaId . snd)
        acc = acc
      -- Otherwise count the occurrences.
      | otherwise = ((length . filter (equalToId media)) mediaIds, media) : acc
        where
          equalToId media = ((fromJust . mediaId) media ==)
          mediaIds = map (fromJust . mediaId) medias

-- | Takes the first amount of anime with most occurences.
mostOccurring :: [(Int, Media)] -> Int -> [(Int, Media)]
mostOccurring medias amount = take amount $ reverse $ sortOn fst medias

-- | Filter the list of selected media to not contain any media that are in
-- list of media given.
nubD :: [Media] ->  [(Int, Media)] -> [(Int, Media)]
nubD givenList = filter notInList
  where
    notInList (_, media) = (fromJust . mediaId) media `notElem` map
      (fromJust . mediaId) givenList

-- | Takes the first amount of anime with the highest global mean score rating.
highestRated :: [(Int, Media)] -> Int -> [(Int, Media)]
highestRated medias amount = take amount $ reverse $ sortOn (mediaMeanScore
  . snd) medias