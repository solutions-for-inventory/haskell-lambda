{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib where

import GHC.Generics
import Data.Aeson
import Aws.Lambda

import Data.Morpheus.Types (GQLRequest, GQLResponse)
import Data.ByteString.Lazy.Internal (ByteString)
import Graphql.Root (api, apiDoc)

data Person = Person
  { personName :: String
  , personAge :: Int
  } deriving (Generic, FromJSON, ToJSON)

--instance FromJSON Person
--instance ToJSON Person

handler :: Person -> Context () -> IO (Either String Person)
handler person context =
  if personAge person > 0 then
    return (Right person)
  else
    return (Left "A person's age must be positive")

gqlHandler :: GQLRequest -> Context () -> IO GQLResponse
gqlHandler gqlRequest context = api gqlRequest

gqlSchemaHandler :: () -> Context () -> IO ByteString
gqlSchemaHandler gqlRequest context = return apiDoc