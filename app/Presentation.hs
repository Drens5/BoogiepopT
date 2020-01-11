{-# LANGUAGE OverloadedStrings #-}

{-| Presentation of results.

Functions that takes results and perhaps fetch more information to present
them in a desired way.

-}

module Presentation
  ( -- * Functions for result presentation
    minimal
  ) where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Query.Datatypes

-- | Maps the media onto it's mediaId.
minimal :: [(Int, Media)] -> [(Int, Int)]
minimal = map toMediaId
  where
    toMediaId (x, y) = (x, (fromJust . mediaId) y)