{-| Functionality to create service values for queries.

Creating service values is wrapping the query and variables in a Service
datatype therefore this is uniform across all queries.

Use this to create services instead of explicitly exporting the value
constructors of the Service datatype.
-}

module Query.Service
  ( -- * Service creation functions.
    createService
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