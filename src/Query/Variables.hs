{-# LANGUAGE OverloadedStrings #-}

{-| Functionality for associated variables to a query.

Provides functions related to each query to create the Map of variables that
the query expects.
The naming convention for them is variablesQueryOperationName.
-}

module Query.Variables
  ( -- * Variable creation functions.
    variablesPsuedoAuthUser
  , variablesUserMediaList
  ) where

import Data.Aeson (Value (..))
import Data.Map.Strict (Map, empty, fromList, singleton)
import Data.Text (Text)
import Data.Scientific (scientific)

-- | A single variable for the name.
variablesPsuedoAuthUser :: Text -> Map Text Value
variablesPsuedoAuthUser name = singleton "name" (String name)

variablesUserMediaList :: Integer -> Integer -> Map Text Value
variablesUserMediaList chunk userId = fromList [("userId", Number (scientific
  userId 0)), ("chunk", Number (scientific chunk 0))]