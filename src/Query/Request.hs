{-# LANGUAGE OverloadedStrings #-}

{-| Requests to fetch information from the anilist Api.

To run (request :: Request) use runReq defaultHttpConfig $ request
(service :: Service) in an IO enabled monad, where Service is the datatype
containing the query and variables.

After running one obtains a Value datatype, one can for instance decode this
to an expected datatype which is an instance of FromJSON using fromJSON.

A single function provides the framework to make the request.
-}

module Query.Request
  ( -- * Use to make requests with.
    request
    -- * Use to make and run requests.
  , runRequest
  , runRequestSafe
  ) where

import Control.Exception (throwIO, try)
import Control.Concurrent (threadDelay)
import Data.Aeson (Value)
import Data.ByteString.Char8 (unpack)
import Data.Maybe (fromJust)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..),
  responseHeaders)
import Network.HTTP.Req
import Network.HTTP.Types.Header (hRetryAfter)
import Query.Datatypes (Service)

type Request = Service -> Req Value

-- | Makes a GraphQL request with the given service, and decodes to a Value.
request :: Request
request service = do
  v <- req POST (https "graphql.anilist.co") (ReqBodyJson service)
    jsonResponse mempty
  return $ responseBody v :: Req Value

-- | Runs a GraphQL request with the given service, and decodes to a Value.
runRequest :: IO Service -> IO Value
runRequest service' = do
  service <- service'
  runReq defaultHttpConfig $ request service

-- | Runs a GraphQL request to the anilist Api with the given service.
-- Upon rate limit: waits the required amount of time and retries the request.
runRequestSafe :: IO Service -> IO Value
runRequestSafe service' = do
  service <- service'
  resp <- try $ runReq defaultHttpConfig $ request service
  case resp of
    Left (VanillaHttpException (HttpExceptionRequest r
      (StatusCodeException response bytestr)))
            -> threadDelay waitTime >> runReq defaultHttpConfig (request
              service)  -- ^ no need to do IO for service again
          where
            waitTime = ((read . unpack . fromJust . lookup hRetryAfter)
              (responseHeaders response) :: Int) * 1000000 -- ^ s -> ms
    Right v -> return v
    -- Left e -> throwIO e  -- ^ rethrow if it's another error