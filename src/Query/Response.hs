{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Response data parsing.

Defines record types in which the record fields are the top level fields of
the data field in the JSON response of a GraphQL request.
Used to parse the response data received, therefore each such record type will
have a FromJSON instance.

There exists a record type for each query.
The naming convention for them is DataQueryOperationName.

There is also a record type for the top level object in which the data field
is present, but which also may have an errors field.
These are parsed as plain Value, after which a data field parser can be applied.
-}

module Query.Response
  ( -- * Top level query response record types.
    TopLevel (..)
  , DataPsuedoAuthUser (..)
  ) where

import Data.Aeson (FromJSON, Value, parseJSON, withObject, (.:?))
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
  } deriving (Show)
instance FromJSON TopLevel where
  parseJSON = withObject "TopLevel" $ \v -> TopLevel
    <$> v .:? "data"
    <*> v .:? "errors"

-- | Top level fields in the data field of the response for the PsuedoAuthUser
-- query operation.
-- Wrapped in Maybe, since if something goes wrong data will be null.
data DataPsuedoAuthUser = DataPsuedoAuthUser
  { psuedoAuthUser :: Maybe User
  } deriving (Generic, Show)
instance FromJSON DataPsuedoAuthUser