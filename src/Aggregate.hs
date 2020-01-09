{-| Aggregating list of media to recommendation.



-}

module Aggregate
  ( -- * Organize lists of media.
    nubC
  ) where

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