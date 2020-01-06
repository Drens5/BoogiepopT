{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Transliterates the relevant fields and datatypes from anilist Api.

Transliteration makes deriving FromJSON instances simple for each datatype,
by making the record type accessor functions equal to the field names.

The thing about GraphQL is that you can specify aliases and decide what fields
you want to have returned on a certain request.
To avoid manual parsing we need to define record types for the fields and in
order to match the flexibility of Graph QL we wrap the type of every record
field accessor function in a Maybe construct.
Note that this allows us to come back later, add a field to the record type,
and any previous code still works without any modifications.
For aliases however one needs to define seperate record types.

It is still required to write a response datatype for a request.
-}

module Query.Datatypes
  ( -- * Has both FromJSON and ToJSON instances.
    Service (..)
  , MediaListStatus (..)
  , MediaFormat (..)
    -- * Only has FromJSON instance.
  , UsersPage (..)
  , PageInfo (..)
  , User (..)
  , UserStatisticTypes (..)
  , UserStatistics (..)
  , UserGenreStatistic (..)
  , UserTagStatistic (..)
  , MediaTag (..)
  , MediaListCollection (..)
  , MediaListGroup (..)
  , MediaList (..)
  , Media (..)
  , MediaTitle (..)
  , MediaStatus (..)
  , MediaTrailer (..)
  , MediaStats (..)
  , ScoreDistribution (..)
  ) where

import Data.Aeson (ToJSON, FromJSON, Value)
import Data.Map.Strict (Map)
import GHC.Generics
import Data.Text (Text)

-- | Type to encapsulate a GraphQL service.
data Service = Service
  { query :: Text
  , variables :: Map Text Value  -- ^ Using Value for Json flexibility.
  } deriving (Generic, Show)
instance ToJSON Service
instance FromJSON Service

-- The Page datatype has many fields, however one can only have one other
-- field than PageInfo in each occurence. The datatype will reflect what kind
-- of page it is, because the behaviour of a Page is very different depending
-- on what kind of page it is.

-- | Page of users.
-- Note that the value constructor now defines the required alias of this field.
-- The Api does not guarantee that these values are non-null.
data UsersPage = UsersPage
  { pageInfo :: Maybe PageInfo
  , users :: Maybe [User]
  } deriving (Generic, Show)
instance FromJSON UsersPage

-- | Contains information about the page datatype.
-- The Api does not guarantee the these values are non-null.
data PageInfo = PageInfo
  { total :: Maybe Int
  , perPage :: Maybe Int
  , currentPage :: Maybe Int
  , lastPage :: Maybe Int
  , hasNextPage :: Maybe Bool
  } deriving (Generic, Show)
instance FromJSON PageInfo

-- | Information of a user.
data User = User
  { id :: Maybe Int  -- ^ Guaranteed non-null on request.
  , name :: Maybe Text  -- ^ Guaranteed non-null on request.
  , statistics :: Maybe UserStatisticTypes
  } deriving (Generic, Show)
instance FromJSON User

-- | Types of userstatistics apparently either anime or manga.
data UserStatisticTypes = UserStatisticTypes
  { anime :: Maybe UserStatistics
  } deriving (Generic, Show)
instance FromJSON UserStatisticTypes

-- | Statistics of a user for anime or manga.
data UserStatistics = UserStatistics
  { count :: Maybe Int  -- ^ Guaranteed non-null on request.
  , minutesWatched :: Maybe Int  -- ^ Guaranteed non-null on request.
  , genres :: Maybe [UserGenreStatistic]
  , tags :: Maybe [UserTagStatistic]
  } deriving (Generic, Show)
instance FromJSON UserStatistics

-- | Statistic on a genre for a user.
data UserGenreStatistic = UserGenreStatistic
  { count :: Maybe Int  -- ^ Guaranteed non-null on request.
  , minutesWatched :: Maybe Int  -- ^ Guaranteed non-null on request.
  , mediaIds :: Maybe [Int]  -- ^ Guaranteed non-null on request. Weird ...!
  , genre :: Maybe Text
  } deriving (Generic, Show)
instance FromJSON UserGenreStatistic

-- | Statistic on a tag for a user.
data UserTagStatistic = UserTagStatistic
  { count :: Maybe Int  -- ^ Guaranteed non-null on request.
  , minutesWatched :: Maybe Int  -- ^ Guaranteed non-null on request.
  , mediaIds :: Maybe [Int]  -- ^ Guaranteed non-null on request. Weird ...!
  , tag :: Maybe MediaTag
  } deriving (Generic, Show)
instance FromJSON UserTagStatistic

-- | Information of a tag.
data MediaTag = MediaTag
  { id :: Maybe Int  -- ^ Guaranteed non-null on request.
  , name :: Maybe Text  -- ^ Guaranteed non-null on request.
  , description :: Maybe Text
  , category :: Maybe Text
  , rank :: Maybe Int  -- ^ w.r.t. this media, null if there is no such context
  } deriving (Generic, Show)
instance FromJSON MediaTag

-- | Information on the anime or manga list of a user.
data MediaListCollection = MediaListCollection
  { lists :: Maybe [MediaListGroup]
  , user :: Maybe User
  , hasNextChunk :: Maybe Bool
  } deriving (Generic, Show)
instance FromJSON MediaListCollection

-- | Header, so to say, for a media list.
data MediaListGroup = MediaListGroup
  { entries :: Maybe [MediaList]
  , name :: Maybe Text
  , status :: Maybe MediaListStatus  -- ^ I don't know the behaviour of this.
    -- Especially on custom lists.
  } deriving (Generic, Show)
instance FromJSON MediaListGroup

-- | Media entry that appears on someone lists.
data MediaList = MediaList
  { id :: Maybe Int  -- ^ Guaranteed non-null on request.
  , userId :: Maybe Int  -- ^ Guaranteeed non-null on request.
  , mediaId :: Maybe Int  -- ^ Guaranteed non-null on request.
  , repeat :: Maybe Int
  , media :: Maybe Media
  , user :: Maybe User
  } deriving (Generic, Show)
instance FromJSON MediaList

-- | Enum of possible status of a media.
-- Gets a ToJSON instance since it can be used as an argument for a query.
data MediaListStatus =
    CURRENT
  | PLANNING
  | COMPLETED
  | DROPPED
  | PAUSED
  | REPEATING
  deriving (Generic, Show)
instance FromJSON MediaListStatus
instance ToJSON MediaListStatus

-- | Information about a certain media entry.
data Media = Media
  { id :: Maybe Int  -- ^ Guaranteed non-null on request.
  , title :: Maybe MediaTitle
  , format :: Maybe MediaFormat
  , status :: Maybe MediaStatus
  , description :: Maybe Text
  , episodes :: Maybe Int
  , trailer :: Maybe MediaTrailer
  , genres :: Maybe [Text]
  , averageScore :: Maybe Int
  , meanScore :: Maybe Int
  , popularity :: Maybe Int
  , tags :: Maybe [MediaTag]
  , stats :: Maybe MediaStats
  } deriving (Generic, Show)
instance FromJSON Media

-- | Official titles of the media in various langauges.
data MediaTitle = MediaTitle
  { romaji :: Maybe Text
  , english :: Maybe Text
  , native :: Maybe Text
  , userPreferred :: Maybe Text
  } deriving (Generic, Show)
instance FromJSON MediaTitle

-- | The format a media can be released in.
-- Gets a ToJSON instance because it's not a compount type.
data MediaFormat =
    TV
  | TV_SHORT
  | MOVIE
  | SPECIAL
  | OVA
  | ONA
  | MUSIC
  | MANGA
  | NOVEL
  | ONE_SHOT
  deriving (Generic, Show)
instance FromJSON MediaFormat
instance ToJSON MediaFormat

-- | The possible releasing status of media.
-- Gets a ToJSON instance because it's not a compount type.
data MediaStatus =
    FINISHED
  | RELEASING
  | NOT_YET_RELEASED
  | CANCELLED
  deriving (Generic, Show)
instance FromJSON MediaStatus
instance ToJSON MediaStatus

data MediaTrailer = MediaTrailer
  { site :: Maybe Text
  } deriving (Generic, Show)
instance FromJSON MediaTrailer

-- | Distribution of the scores of the anime.
data MediaStats = MediaStats
  { scoreDistribution :: Maybe [ScoreDistribution]
  } deriving (Generic, Show)
instance FromJSON MediaStats

-- | Amount of list entries with that score.
data ScoreDistribution = ScoreDistribution
  { score :: Maybe Int
  , amount :: Maybe Int
  } deriving (Generic, Show)
instance FromJSON ScoreDistribution
