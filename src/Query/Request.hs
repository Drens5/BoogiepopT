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
  ) where

import Data.Aeson (Value)
import Network.HTTP.Req
import Query.Datatypes (Service)

type Request = Service -> Req Value

-- | Makes a GraphQL request with the given service, and decodes to a Value.
request :: Request
request service = do
  v <- req POST (https "graphql.anilist.co") (ReqBodyJson service)
    jsonResponse mempty
  return $ responseBody v :: Req Value