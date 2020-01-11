{-# LANGUAGE OverloadedStrings #-}

{-| Interface for using the application as a user.

Provides top level control flow and user interaction on using the application.

-}

module Interface
  ( -- *
    psuedoAuthenticateUser
  ) where

import Data.Maybe (fromJust)
import Prelude hiding ()
import Data.Text (Text)
import Data.Text.IO ()
import Query.Datatypes
import Query.Request (runRequestSafe)
import Query.Response (DataPsuedoAuthUser (..), fromSuccess, dataPsuedoAuthUser)
import Query.Service (servicePsuedoAuthUser)

-- | Interacts with the application user for his anilist username to fetch
-- their user data.
psuedoAuthenticateUser :: Text -> IO User
psuedoAuthenticateUser name = do
  r <- runRequestSafe (servicePsuedoAuthUser name)
  let pAuth = (fromSuccess . getUser) r
  return pAuth

  where
    getUser r = (fromJust . psuedoAuthUser) <$> dataPsuedoAuthUser r