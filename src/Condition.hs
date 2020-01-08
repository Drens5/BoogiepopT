{-# LANGUAGE OverloadedStrings #-}

{-| Functions that specify a property that needs to hold for an anime.

A condition is essentially a function D :: Media -> ??? -> Bool which
determines whether a certain anime satisfies a certain property.

-}

module Condition
  ( -- *
    Tag (..)
    -- * Condition functions
  , tagCondition
  ) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Query.Datatypes

-- | Wrapper for tags to specify whether it is strong or weakly coupled.
-- For weakly coupled tags a property has to hold for at least on of the tags
-- in the collection.
data Tag =
    Strong Text
  | Weak [Text]

-- | Condition of whether the media has the given tag.
-- The field mediaTagFields must be present.
tagCondition :: Media -> Tag -> Bool
tagCondition media (Strong tag) = elem tag $ map (fromJust . mediaTagName) $
  (fromJust . mediaTags) media
tagCondition media (Weak tags) = head $ foldr step [] tags
  where
    step tag [] = [tagCondition media (Strong tag)]
    step tag acc@(x:_)
      | x = acc  -- ^ a previous tag succeeded, do nothing
      | otherwise = tagCondition media (Strong tag) : acc
