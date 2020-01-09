{-# LANGUAGE OverloadedStrings #-}

{-| Primitive recommendation algorithm based on coupled genres and tags.

Recco is a module in which there are submodules which define the core of a
recommendation method.

CoupledGT decides on a recommendation by aggregating anime of certain genres
and tags from a collection of users.
The users that are selected must have a sufficient amount of anime veterancy
as compared to PAuthUser (the user seekign a recommendation), both globally as
with the tags and genres specified.

Global anime veterancy is determined by the number of anime watched and the
amount of minutes watched as a whole.
Veterancy of the tags and genres is determined by the number of anime watched
of that genre or containing that tag.
-}

module Recco.CoupledGT
  ( -- *

  ) where

import Condition (Coupled (..), Tag, Genre, tagsCondition, genresCondition)
import Criteria (watchedCriteria, timeWatched, animeCount)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Query.Datatypes
import Query.Request (request)
import Query.Service (servicePsuedoAuthUser, serviceUserMediaList)

-- | Gives a list of recommended media, using the primitive coupledGT method.
coupledGT :: User -> [Coupled Genre] -> [Coupled Tag] -> [Media]
coupledGT pAuth genres tags =
  let pAuthTimeWatched = timeWatched pAuth
      pAuthAnimeCount = animeCount pAuth
      pAuthConditionAnimeCount = undefined -- ^ Something like this.
   in undefined
  where
    y = undefined

-- | k is the amount of users that we must select, n is the amount of users
-- that have so far been selected, m is the amount of anime that have to
-- satisfy the total condition.
coupledGTCore :: User -> [Coupled Genre] -> [Coupled Tag] -> Int -> Int ->
  Int -> [Media] -> [Media]
coupledGTCore pAuth gs ts m k n acc = undefined