{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib where

import RIO hiding (ByteString)
import GHC.Generics
import Data.Aeson
import Aws.Lambda

import Data.Morpheus.Types (GQLRequest, GQLResponse)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)
import Graphql.Root (api, apiDoc)

data Person = Person
  { personName :: String
  , personAge :: Int
  } deriving (Generic, FromJSON, ToJSON)

data SchemaGQL = SchemaGQL
  { schema :: String
  } deriving (Generic, FromJSON, ToJSON)

--instance FromJSON Person
--instance ToJSON Person

handler :: Person -> Context () -> IO (Either String Person)
handler person context =
  if personAge person > 0 then
    return (Right person)
  else
    return (Left "A person's age must be positive")

gqlHandler :: GQLRequest -> Context () -> IO (Either String GQLResponse)
gqlHandler gqlRequest context = do
                                 gqlResponse <- api gqlRequest
                                 return $ Right gqlResponse

gqlSchemaHandler :: () -> Context () -> IO (Either String String)
gqlSchemaHandler _ context = pure $ Right $ unpack apiDoc