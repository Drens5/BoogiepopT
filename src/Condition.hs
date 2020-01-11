{-| Functions that specify a property that needs to hold for an anime.

A condition is essentially a function D :: Media -> ??? -> Bool which
determines whether a certain anime satisfies a certain property.

-}

module Condition
  ( -- *
    Coupled (..)
  , Tag
  , Genre
    -- * Condition functions
  , tagCondition
  , tagsCondition
  , genreCondition
  , genresCondition
  ) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Query.Datatypes

-- | Wrapper for tags and genres to specify whether it is strong or weakly
-- coupled. For weakly coupled tags a property has to hold for at least on
-- of the tags in the collection.
data Coupled a =
    Strong a
  | Weak [a]
  deriving (Read, Show)

type Genre = Text
type Tag = Text

-- | Condition of whether the media has the given tag.
-- The field mediaTagFields must be present.
tagCondition :: Media -> Coupled Tag -> Bool
tagCondition media (Strong tag) = elem tag $ map (fromJust . mediaTagName) $
  (fromJust . mediaTags) media
tagCondition media (Weak tags) = head $ foldr step [] tags
  where
    step tag [] = [tagCondition media (Strong tag)]
    step tag acc@(x:_)
      | x = acc  -- ^ a previous tag succeeded, do nothing
      | otherwise = tagCondition media (Strong tag) : acc

-- | Does the media have all the given tags, in a coupled sense.
tagsCondition :: [Coupled Tag] -> Media -> Bool
tagsCondition ts media = all (tagCondition media) ts

-- | Condition of whether the media has the given genre, same as tagCondition.
genreCondition :: Media -> Coupled Genre -> Bool
genreCondition media (Strong genre) = elem genre $ (fromJust . mediaGenres) media
genreCondition media (Weak genres) = head $ foldr step [] genres
  where
    step genre [] = [genreCondition media (Strong genre)]
    step genre acc@(x:_)
      | x = acc -- ^ a previous genre is present, do nothing
      | otherwise = genreCondition media (Strong genre) : acc

-- | Does the media have all the given genres, in a coupled sense.
genresCondition :: [Coupled Genre] -> Media -> Bool
genresCondition gs media = all (genreCondition media) gs