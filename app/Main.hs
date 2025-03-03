module Main where

import RIO
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson (encode)
import Prelude (putStrLn)
import Data.Maybe (fromJust)
import Aws.Lambda (runLambdaHaskellRuntime, defaultDispatcherOptions, addStandaloneLambdaHandler, Context(..))
import Lib (gatewayHandler)
import Config
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text.Lazy.Encoding as TLE
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404, methodPost)
import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Graphql.Root (api, apiDoc)
import Data.Morpheus.Types (GQLRequest, GQLResponse)

main :: IO ()
main = do
  appConfig <- getAppConfig
  if "web" == appType appConfig then
    execWebServer appConfig
  else
    execRuntimeLambda
execRuntimeLambda :: IO ()
execRuntimeLambda = runLambdaHaskellRuntime
  defaultDispatcherOptions
  (pure ())
  id $ do
--    addAPIGatewayHandler "api-gateway" gatewayHandler
    addStandaloneLambdaHandler "handler" gatewayHandler

execWebServer :: AppConfig -> IO ()
execWebServer AppConfig{..} = scotty port $ do
  middleware logStdoutDev
  get "/graphql" $ do
    text $ TLE.decodeUtf8 apiDoc
  post "/graphql" $ do
    body <- body
    let inputPayload = decode body :: Maybe GQLRequest
    apiResponse <- liftIO $ api $ fromJust inputPayload
    json apiResponse
  notFound $ do
    status status404
    text "404 - Not Found"

--dummyContext ref = Context {
--        functionName = "functionName",
--        functionVersion = "1.0",
--        logStreamName = "logStream",
--        logGroupName = "logGroup",
--        memoryLimitInMb = 128,
--        invokedFunctionArn = "mempty",
--        xrayTraceId = "mempty",
--        awsRequestId = "mempty",
--        deadline = 0,
--        customContext = ref
--      }