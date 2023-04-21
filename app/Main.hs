module Main where

import RIO
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Prelude (putStrLn)
import Data.Maybe (fromJust)
import Aws.Lambda
import Lib (handler, gqlHandler, gqlSchemaHandler, gatewayHandler)
import Lib (ApiRequest(..))
import Config
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
--import Init (runAppDevel)
--import Models (migrateDB)

main :: IO ()
main = do
  appConfig <- getAppConfig
  if "local" == env appConfig then
    runLocal
  else
--  migrateDB
   (
      runLambdaHaskellRuntime
        defaultDispatcherOptions
        (pure ())
        id $ do
    --    (addAPIGatewayHandler "api-gateway" gatewayHandler)
        (addStandaloneLambdaHandler "handler" gatewayHandler)
    )

runLocal :: IO ()
runLocal = do
    ref <- newIORef 0
    apiRequest <- (decode <$> B.readFile "config/mock/api_request.json") :: IO (Maybe ApiRequest)
    apiResponse <- gatewayHandler (fromJust apiRequest) (dummyContext ref)
    B.writeFile "dist/response.json" (encodePretty $ apiResponse)
--    encodeFile "dist/response.json" apiResponse
--    putStrLn $ C.unpack $ encode $ apiResponse
    return ()

dummyContext ref = Context {
        functionName = "functionName",
        functionVersion = "1.0",
        logStreamName = "logStream",
        logGroupName = "logGroup",
        memoryLimitInMb = 128,
        invokedFunctionArn = "mempty",
        xrayTraceId = "mempty",
        awsRequestId = "mempty",
        deadline = 0,
        customContext = ref
      }