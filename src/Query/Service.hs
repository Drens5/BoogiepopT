{-| Functionality to create service values for queries.

Creating service values is wrapping the query and variables in a Service
datatype therefore this is uniform across all queries.

However this module provides handy functions which allow to make a service by
passing in the values for the required variables for the query.
Hence, make doing top level requests easier.

Naming convention: serviceQueryOperationName.
-}

module Query.Service
  ( -- * Service creation functions.
    servicePsuedoAuthUser
  , serviceUserMediaList
  , serviceArbitraryUsers
  ) where

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Query.Datatypes (Service (..))
import Query.Load
import Query.Variables

-- | Wraps query and variables in a Service.
-- The order of the arguments is as such, since the query will be coming from
-- the IO monad. It's easier to use with fmap.
createService :: Map Text Value -> Text -> Service
createService variables query = Service query variables

-- | Create a service for the PsuedoAuthUser query operation by passing the
-- variable values for this operation.
servicePsuedoAuthUser :: Text -> IO Service
servicePsuedoAuthUser name = createService (variablesPsuedoAuthUser
  name) <$> loadPsuedoAuthUser

serviceUserMediaList :: Integer -> Integer -> IO Service
serviceUserMediaList chunk userId = createService (variablesUserMediaList
  chunk userId) <$> loadUserMediaList

serviceArbitraryUsers :: Integer -> IO Service
serviceArbitraryUsers page = createService (variablesArbitraryUsers page) <$>
  loadArbitraryUsers