module Main where

import RIO
import Aws.Lambda
import Lib (handler, gqlHandler)
--import Init (runAppDevel)
--import Models (migrateDB)

main :: IO ()
main = do
--  migrateDB
  runLambdaHaskellRuntime
    defaultDispatcherOptions
    (pure ())
    id
    (addStandaloneLambdaHandler "handler" handler)
