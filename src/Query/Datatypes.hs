{-# LANGUAGE DeriveGeneric     #-}

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

To avoid nameclashes record fields will be prefixed by an indicator of the
type. This corresponds to needing the query to have these names as aliases.
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
import Data.Text (Text)
import GHC.Generics

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
  { userPageInfo :: Maybe PageInfo
  , userPageUsers :: Maybe [User]
  } deriving (Generic, Eq, Show)
instance FromJSON UsersPage

-- | Contains information about the page datatype.
-- The Api does not guarantee the these values are non-null.
data PageInfo = PageInfo
  { pageInfoTotal :: Maybe Int
  , pageInfoPerPage :: Maybe Int
  , pageInfoCurrentPage :: Maybe Int
  , pageInfoLastPage :: Maybe Int
  , pageInfoHasNextPage :: Maybe Bool
  } deriving (Generic, Eq, Show)
instance FromJSON PageInfo

-- | Information of a user.
data User = User
  { userId :: Maybe Int  -- ^ Guaranteed non-null on request.
  , userName :: Maybe Text  -- ^ Guaranteed non-null on request.
  , userSiteUrl :: Maybe Text
  , userStatistics :: Maybe UserStatisticTypes
  } deriving (Generic, Eq, Show)
instance FromJSON User

-- | Types of userstatistics apparently either anime or manga.
data UserStatisticTypes = UserStatisticTypes
  { userStatisticTypesAnime :: Maybe UserStatistics
  } deriving (Generic, Eq, Show)
instance FromJSON UserStatisticTypes

-- | Statistics of a user for anime or manga.
data UserStatistics = UserStatistics
  { userStatisticsCount :: Maybe Int  -- ^ Guaranteed non-null on request.
  , userStatisticsMinutesWatched :: Maybe Int  -- ^ Guaranteed non-null on request.
  , userStatisticsGenres :: Maybe [UserGenreStatistic]
  , userStatisticsTags :: Maybe [UserTagStatistic]
  } deriving (Generic, Eq, Show)
instance FromJSON UserStatistics

-- | Statistic on a genre for a user.
data UserGenreStatistic = UserGenreStatistic
  { userGenreStatisticCount :: Maybe Int  -- ^ Guaranteed non-null on request.
  , userGenreStatisticMinutesWatched :: Maybe Int  -- ^ Guaranteed non-null on request.
  , userGenreStatisticMediaIds :: Maybe [Int]  -- ^ Guaranteed non-null on request. Weird ...!
  , userGenreStatisticGenre :: Maybe Text
  } deriving (Generic, Eq, Show)
instance FromJSON UserGenreStatistic

-- | Statistic on a tag for a user.
data UserTagStatistic = UserTagStatistic
  { userTagStatisticCount :: Maybe Int  -- ^ Guaranteed non-null on request.
  , userTagStatisticMinutesWatched :: Maybe Int  -- ^ Guaranteed non-null on request.
  , userTagStatisticMediaIds :: Maybe [Int]  -- ^ Guaranteed non-null on request. Weird ...!
  , userTagStatisticTag :: Maybe MediaTag
  } deriving (Generic, Eq, Show)
instance FromJSON UserTagStatistic

-- | Information of a tag.
data MediaTag = MediaTag
  { mediaTagId :: Maybe Int  -- ^ Guaranteed non-null on request.
  , mediaTagName :: Maybe Text  -- ^ Guaranteed non-null on request.
  , mediaTagDescription :: Maybe Text
  , mediaTagCategory :: Maybe Text
  , mediaTagRank :: Maybe Int  -- ^ w.r.t. this media, null if there is no such context
  , mediaTagIsMediaSpoiler :: Maybe Bool  -- ^ same remark as above
  } deriving (Generic, Eq, Show)
instance FromJSON MediaTag

-- | Information on the anime or manga list of a user.
data MediaListCollection = MediaListCollection
  { mediaListCollectionLists :: Maybe [MediaListGroup]
  , mediaListCollectionUser :: Maybe User
  , mediaListCollectionHasNextChunk :: Maybe Bool
  } deriving (Generic, Eq, Show)
instance FromJSON MediaListCollection

-- | Header, so to say, for a media list.
data MediaListGroup = MediaListGroup
  { mediaListGroupEntries :: Maybe [MediaList]
  , mediaListGroupName :: Maybe Text
  , mediaListGroupStatus :: Maybe MediaListStatus  -- ^ I don't know the behaviour of this.
    -- Especially on custom lists.
  } deriving (Generic, Eq, Show)
instance FromJSON MediaListGroup

-- | Media entry that appears on someone lists.
data MediaList = MediaList
  { mediaListId :: Maybe Int  -- ^ Guaranteed non-null on request.
  , mediaListUserId :: Maybe Int  -- ^ Guaranteeed non-null on request.
  , mediaListMediaId :: Maybe Int  -- ^ Guaranteed non-null on request.
  , mediaListStatus :: Maybe MediaListStatus
  , mediaListRepeat :: Maybe Int
  , mediaListMedia :: Maybe Media
  , mediaListUser :: Maybe User
  } deriving (Generic, Eq, Show)
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
  deriving (Generic, Eq, Show)
instance FromJSON MediaListStatus
instance ToJSON MediaListStatus

-- | Information about a certain media entry.
data Media = Media
  { mediaId :: Maybe Int  -- ^ Guaranteed non-null on request.
  , mediaTitle :: Maybe MediaTitle
  , mediaFormat :: Maybe MediaFormat
  , mediaStatus :: Maybe MediaStatus
  , mediaDescription :: Maybe Text
  , mediaEpisodes :: Maybe Int
  , mediaTrailer :: Maybe MediaTrailer
  , mediaGenres :: Maybe [Text]
  , mediaAverageScore :: Maybe Int
  , mediaMeanScore :: Maybe Int
  , mediaPopularity :: Maybe Int
  , mediaTags :: Maybe [MediaTag]
  , mediaStats :: Maybe MediaStats
  } deriving (Generic, Eq, Show)
instance FromJSON Media

-- | Official titles of the media in various langauges.
data MediaTitle = MediaTitle
  { mediaTitleRomaji :: Maybe Text
  , mediaTitleEnglish :: Maybe Text
  , mediaTitleNative :: Maybe Text
  , mediaTitleUserPreferred :: Maybe Text
  } deriving (Generic, Eq, Show)
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
  deriving (Generic, Eq, Show)
instance FromJSON MediaFormat
instance ToJSON MediaFormat

-- | The possible releasing status of media.
-- Gets a ToJSON instance because it's not a compount type.
data MediaStatus =
    FINISHED
  | RELEASING
  | NOT_YET_RELEASED
  | CANCELLED
  deriving (Generic, Eq, Show)
instance FromJSON MediaStatus
instance ToJSON MediaStatus

data MediaTrailer = MediaTrailer
  { mediaTrailerSite :: Maybe Text
  } deriving (Generic, Eq, Show)
instance FromJSON MediaTrailer

-- | Distribution of the scores of the anime.
data MediaStats = MediaStats
  { mediaStatsScoreDistribution :: Maybe [ScoreDistribution]
  } deriving (Generic, Eq, Show)
instance FromJSON MediaStats

-- | Amount of list entries with that score.
data ScoreDistribution = ScoreDistribution
  { scoreDistributionScore :: Maybe Int
  , scoreDistributionAmount :: Maybe Int
  } deriving (Generic, Eq, Show)
instance FromJSON ScoreDistribution
