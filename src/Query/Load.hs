{-# LANGUAGE OverloadedStrings #-}

{-| Load queries from the files in which they are written.

When defining a query one can freely add top level aliases, because an
associated top level record type has to be created to do the parsing.
This record type can then match the aliases.

The files are read strictly.
Moreover the files read are available as top level actions.

The naming convention for these top level actions is loadQueryOperationName.
-}

module Query.Load
  ( -- * Actions which load a query
   loadPsuedoAuthUser
  ) where

import Prelude hiding (readFile)
import Data.Text (Text)
import Data.Text.IO (readFile)

-- | Query that gets relevant information of the user that is seeking a
-- recommendation.
loadPsuedoAuthUser :: IO Text
loadPsuedoAuthUser = readFile "src/Query/Queries/PsuedoAuthUser.qr"