{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib where

import Prelude (putStr)
import RIO hiding (ByteString)
import GHC.Generics
import Data.Aeson
import Aws.Lambda
import Data.Text (pack)
import Data.Maybe (fromJust)
import Data.Aeson.Key (fromText)
import Data.Morpheus.Types (GQLRequest, GQLResponse)
--import Data.ByteString.Lazy.Internal (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)
import Graphql.Root (api, apiDoc)

data Person = Person
  { personName :: String
  , personAge :: Int
  } deriving (Generic, FromJSON, ToJSON)

data SchemaGQL = SchemaGQL
  { schema :: String
  } deriving (Generic, FromJSON, ToJSON)


data Header = Header {headerName :: Text, value :: Text} deriving (Generic)
instance ToJSON Header where
  toJSON (Header { headerName = headerName, value = value }) =
    object [ fromText headerName .= value ]
data ApiResponse = ApiResponse  { isBase64Encoded :: Bool, statusCode :: Int, body :: Text, headers :: [Header] } deriving (Generic, ToJSON)

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

gatewayHandler :: () -> Context () -> IO (Either String ApiResponse)
gatewayHandler request context = do
                                let schemaDoc = pack $ unpack apiDoc
--                                gqlResponse <- api $ fromJust $ apiGatewayRequestBody request
                                let response = ApiResponse  { isBase64Encoded = False, statusCode = 200, body = schemaDoc, headers = [Header "Content-Type" "text/plain"]}
                                return $ Right $ response

--gatewayHandler :: ApiGatewayRequest GQLRequest -> Context context -> IO (Either (ApiGatewayResponse error) (ApiGatewayResponse GQLResponse))
--gatewayHandler request context = do
--                                let schemaDoc = pack $ unpack apiDoc
--                                gqlResponse <- api $ fromJust $ apiGatewayRequestBody request
--                                let response = mkApiGatewayResponse 200 [("Content-Type", "text/plain")] $ gqlResponse
--                                return $ Right $ response