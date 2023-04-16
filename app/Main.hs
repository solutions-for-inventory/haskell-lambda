module Main where

import RIO
import Aws.Lambda (runLambdaHaskellRuntime, addAPIGatewayHandler, defaultDispatcherOptions, addStandaloneLambdaHandler)
import Lib (handler, gqlHandler, gqlSchemaHandler, gatewayHandler)
--import Init (runAppDevel)
--import Models (migrateDB)

main :: IO ()
main = do
--  migrateDB
  runLambdaHaskellRuntime
    defaultDispatcherOptions
    (pure ())
    id $ do
--    (addAPIGatewayHandler "api-gateway" gatewayHandler)
    (addStandaloneLambdaHandler "handler" gatewayHandler)

