module Main where

import RIO
import Aws.Lambda
import qualified Lib
--import Init (runAppDevel)
--import Models (migrateDB)

main :: IO ()
main = do
--  migrateDB
  runLambdaHaskellRuntime
    defaultDispatcherOptions
    (pure ())
    id
    (addStandaloneLambdaHandler "handler" Lib.gqlHandler)
