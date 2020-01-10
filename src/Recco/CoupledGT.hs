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
  ( -- * Recommendation algorithms
    coupledGT
  ) where

import Aggregate (nubC, mostOccurring, nubD, highestRated)
import Condition (Coupled (..), Tag, Genre, tagsCondition, genresCondition)
import Control.Exception (NonTermination (..), throwIO)
import Control.Monad (filterM)
import Criteria (watchedCriteria, timeWatched, animeCount)
import Data.Aeson (Result (..))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Query.Datatypes
import Query.Request (runRequestSafe)
import Query.Response (DataUserMediaList (..), DataArbitraryUsers (..),
  dataUserMediaList, dataArbitraryUsers)
import Query.Service (servicePsuedoAuthUser, serviceUserMediaList,
  serviceArbitraryUsers)
import System.Random (getStdRandom, randomR)

-- | Gives a list of recommended media, using the primitive coupledGT method.
coupledGT :: User -> [Coupled Genre] -> [Coupled Tag] -> Int -> IO [(Int, Media)]
coupledGT pAuth genres tags minUsersConsidered =
  let pAuthTimeWatched = timeWatched pAuth
      pAuthAnimeCount = animeCount pAuth
    in do
      pAuthUserList <- verifyUser genres tags 0 pAuth
      let pAuthConditionAnimeCount = length pAuthUserList
      selectedAnimeRaw <- coupledGTCore pAuth genres tags
        pAuthConditionAnimeCount minUsersConsidered [] []
      let selectedAnimeMinAgg = nubC selectedAnimeRaw
      let selectedAnimeMinAggFiltered = nubD pAuthUserList selectedAnimeMinAgg
      return $ mostOccurring selectedAnimeMinAggFiltered 3 ++ highestRated
        selectedAnimeMinAggFiltered 3  -- ^ 3 is arbitrary, change at will.

-- |
-- k is the amount of users that we have yet to select m is the amount of anime that have to
-- satisfy the total condition.
coupledGTCore :: User -> [Coupled Genre] -> [Coupled Tag] -> Int -> Int ->
  [Int] -> [Media] -> IO [Media]
coupledGTCore pAuth gs ts m k seen acc
  | k <= 0 = return acc
  | otherwise = do
    -- Query to find the amount of UserPages
    response <- runRequestSafe (serviceArbitraryUsers 1)
    let lastPage = (fromSuccess . getLastPage) response

    page <- unprecedentedRandomNumber lastPage seen 0

    -- Current round of users to be tested.
    usersResponse <- runRequestSafe (serviceArbitraryUsers (toInteger page))
    let users = (fromSuccess . getUsers) usersResponse

    -- Prelimenary test for global anime vetarancy.
    let passedPrelimUsers = filter (watchedCriteria pAuth) users

    -- Users that passed the preliminaries are tested against the rest of the
    -- conditions. For those who pass their anime are considered otherwise not.
    -- Requires Api query.
    let intermediate = map (verifyUser gs ts m) passedPrelimUsers

    -- Amount of users whose anime list has been taking in consideration,
    -- remove empty lists and then count.
    l <- foldr step2 (return []) intermediate
    let selected = length l


    -- The anime of the selected users, concat the list.
    let considered = foldr step (return []) intermediate

    considered' <- considered
    -- Continue until we have selected enough people.
    coupledGTCore pAuth gs ts m (k - selected) (page : seen) (considered' ++ acc)

    where
      getLastPage r = (fromJust . pageInfoLastPage . fromJust . userPageInfo .
        fromJust . arbitraryUsers) <$> dataArbitraryUsers r
      getUsers r = (fromJust . userPageUsers . fromJust . arbitraryUsers) <$>
        dataArbitraryUsers r
      step listInIO accIO = do
        list <- listInIO
        acc' <- accIO
        return $ list ++ acc'
      step2 listInIO accIO = do
        list <- listInIO
        acc' <- accIO
        case list of
          [] -> return acc' -- list is empty do nothing.
          _  -> return $ list : acc' -- list is not empty, add again.

fromSuccess :: Result a -> a
fromSuccess (Error s) = errorWithoutStackTrace $ "Result.fromSucces: Error" ++ s
fromSuccess (Success x) = x

-- | Generate a random number that does not appear in the list of exempt ones.
unprecedentedRandomNumber :: Int -> [Int] -> Int -> IO Int
unprecedentedRandomNumber ub exempt considered
  | considered /= 0 || considered `notElem` exempt = return considered
  | length exempt >= ub = throwIO NonTermination  -- ^ There are not enough
    -- users satisfying your criteria. Needs a more proper implementation.
  | otherwise = do
    c <- getStdRandom (randomR (1, ub))
    unprecedentedRandomNumber ub exempt c

-- | Returns a list of media satisfying the genre and tag conditions, that is
-- the user's media list contains at least m media containing the genres and
-- tags given in a coupled sense.
-- If this is not the case an empty list is returned.
verifyUser :: [Coupled Genre] -> [Coupled Tag] -> Int -> User -> IO [Media]
verifyUser gs ts m user = do
  responseHasNextChunk <- runRequestSafe (serviceUserMediaList (toInteger 1)
    (toInteger ((fromJust . userId) user)))
  let hasNextChunk = (fromSuccess . getHasNextChunk) responseHasNextChunk
  verifyUserInner gs ts m user 1 hasNextChunk []

getHasNextChunk r = (fromJust . mediaListCollectionHasNextChunk . fromJust .
    userMediaList) <$> dataUserMediaList r

verifyUserInner :: [Coupled Genre] -> [Coupled Tag] -> Int -> User -> Int -> Bool ->
  [Media] -> IO [Media]
verifyUserInner gs ts m user chunk hasNextChunk save
  | hasNextChunk = do
    mediaSatisfyingTotalCondition <- filterToMediaSatisfyingTotalCondition gs
      ts user chunk

    -- Is there a next chunk after the chunk that we're going to do next?
    rHasNextChunkOnNext <- runRequestSafe (serviceUserMediaList (toInteger
      (chunk + 1)) (toInteger ((fromJust . userId) user)))
    let hasNextChunkOnNext = (fromSuccess . getHasNextChunk) rHasNextChunkOnNext

    -- Go for the next one, and take with you the ones that you've gatherd up
    -- 'till now.
    verifyUserInner gs ts m user (chunk + 1) hasNextChunkOnNext
      (mediaSatisfyingTotalCondition ++ save)

  | otherwise = do
    mediaSatisfyingTotalCondition <- filterToMediaSatisfyingTotalCondition gs
      ts user chunk
    case length mediaSatisfyingTotalCondition of
      amountSatisfying
        | amountSatisfying >= m -> return $ mediaSatisfyingTotalCondition ++ save
        | amountSatisfying < m -> return []  -- ^ with this implementation we
        -- can count the users that satisfied all the conditions.

-- | Filter to the media that satisfies all the genres and tags in a coupled
-- sense within a chunk.
filterToMediaSatisfyingTotalCondition :: [Coupled Genre] -> [Coupled Tag]
  -> User -> Int -> IO [Media]
filterToMediaSatisfyingTotalCondition gs ts user chunk = do
  rLists <- runRequestSafe (serviceUserMediaList (toInteger chunk) (toInteger
    ((fromJust . userId) user)))
  let lists = (fromSuccess . getLists) rLists

  -- Get every fetched media as it's on the list flattened in one list.
  let mediaOnList = concatMap (fromJust . mediaListGroupEntries) lists

  -- Map it to a list of the media solely.
  let medias = map (fromJust . mediaListMedia) mediaOnList

  return $ filter (tagsCondition ts) (filter (genresCondition gs) medias)
  where
    getLists r = (fromJust . mediaListCollectionLists . fromJust .
      userMediaList) <$> dataUserMediaList r