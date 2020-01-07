{-# LANGUAGE OverloadedStrings #-}

{-| Functionality for associated variables to a query.

Provides functions related to each query to create the Map of variables that
the query expects.
The naming convention for them is variablesQueryOperationName.
-}

module Query.Variables
  ( -- * Variable creation functions.
    variablesPsuedoAuthUser
  ) where

import Data.Aeson (Value (..))
import Data.Map.Strict (Map, empty, fromList, singleton)
import Data.Text (Text)

-- | A single variable for the name.
variablesPsuedoAuthUser :: Text -> Map Text Value
variablesPsuedoAuthUser name = singleton "name" (String name)