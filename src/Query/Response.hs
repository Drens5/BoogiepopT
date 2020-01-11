{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Response data parsing.

Defines record types in which the record fields are the top level fields of
the data field in the JSON response of a GraphQL request.
Used to parse the response data received, therefore each such record type will
have a FromJSON instance.

There is a record type for the top level object in which the data field
is present, but which also may have an errors field.
These are parsed as plain Value, after which a data field parser can be applied.

There exists a record type for each query.
The naming convention for them is DataQueryOperationName.

These record types each come with an associated function to parse the data
field from a GraphQL JSON response.
The naming convention for these are dataQueryOperationName.

-}

module Query.Response
  ( -- * Top level query response record types.
    TopLevel (..)
  , DataPsuedoAuthUser (..)
  , DataUserMediaList (..)
  , DataArbitraryUsers (..)
    -- * Functions to extract the data field from a response.
  , fromSuccess
  , dataPsuedoAuthUser
  , dataUserMediaList
  , dataArbitraryUsers
  ) where

import Data.Aeson (FromJSON, Result (..), Value, fromJSON, parseJSON, withObject,
  (.:?))
import Data.Maybe (fromJust)
import Data.Text (Text)
import GHC.Generics
import Query.Datatypes

-- | Parse the top level object which contains a data field and may contain an
-- errors field.
-- The FromJSON instance is provided manually because data is a keyword in
-- haskell.
data TopLevel = TopLevel
  { topLevelData :: Maybe Value  -- ^ Can be null.
  , topLevelError :: Maybe Value
  } deriving (Eq, Show)
instance FromJSON TopLevel where
  parseJSON = withObject "TopLevel" $ \v -> TopLevel
    <$> v .:? "data"
    <*> v .:? "errors"

-- | Parse the errors field from a response of a GraphQL service request.
-- The errors field has to be confirmed non-null
parseError :: Value -> Result Value
parseError obj = (fromJSON obj :: Result TopLevel) >>=
  (fromJSON . fromJust . topLevelError)

-- | Extract the value wrapped in a Success value constructor.
-- Throws an error if the value is wrapper in an Error value constructor.
fromSuccess :: Result a -> a
fromSuccess (Error s) = errorWithoutStackTrace $ "Result.fromSucces: Error" ++ s
fromSuccess (Success x) = x

-- | Top level fields in the data field of the response for the PsuedoAuthUser
-- query operation.
-- Wrapped in Maybe, since if something goes wrong data will be null.
data DataPsuedoAuthUser = DataPsuedoAuthUser
  { psuedoAuthUser :: Maybe User
  } deriving (Generic, Eq, Show)
instance FromJSON DataPsuedoAuthUser

-- | Parse the data field for the PsuedoAuthUser query operation, from the
-- object received as response.
-- The data field has to be confirmed non-null.
dataPsuedoAuthUser :: Value -> Result DataPsuedoAuthUser
dataPsuedoAuthUser obj = (fromJSON obj :: Result TopLevel) >>=
  (fromJSON . fromJust . topLevelData)

-- | Top level fields in the data field of the response for the UserMediaList
-- query operation.
data DataUserMediaList = DataUserMediaList
  { userMediaList :: Maybe MediaListCollection
  } deriving (Generic, Eq, Show)
instance FromJSON DataUserMediaList

-- | Parse the data field for the UserMediaList query operation, from the
-- object received as response.
-- The data field has to be confirmed non-null.
dataUserMediaList :: Value -> Result DataUserMediaList
dataUserMediaList obj = (fromJSON obj :: Result TopLevel) >>=
  (fromJSON . fromJust . topLevelData)

-- | Top level fields in thedata field of the response for the ArbitraryUsers
-- query operation.
data DataArbitraryUsers = DataArbitraryUsers
  { arbitraryUsers :: Maybe UsersPage
  } deriving (Generic, Eq, Show)
instance FromJSON DataArbitraryUsers

-- | Parse the data field for the ArbitraryUsers query operation, from the
-- object received as response.
dataArbitraryUsers :: Value -> Result DataArbitraryUsers
dataArbitraryUsers obj = (fromJSON obj :: Result TopLevel) >>=
  (fromJSON . fromJust . topLevelData)