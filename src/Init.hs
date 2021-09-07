{-# LANGUAGE BangPatterns, OverloadedStrings #-}


module Init where

import Data.Typeable
import qualified Data.Text as Text
import Data.Text (Text)
import Control.Monad.Logger
import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Control.Concurrent (killThread)
--import qualified Control.Monad.Metrics as M
import Database.Persist.Postgresql (runMigration, runSqlPersistMPool, withPostgresqlPool, runSqlPool, ConnectionString)
--import Lens.Micro ((^.))
--import Network.Wai (Application)
--import Network.Wai.Metrics (metrics, registerWaiMetrics)
import System.Environment (lookupEnv)
--import System.Remote.Monitoring (forkServer, serverMetricStore, serverThreadId)
--import Say
import Data.Monoid
import Control.Exception.Safe

--import Api (app)
--import Api.User (generateJavaScript)
--import Config (Config(..), Environment(..), makePool, setLogger)
--import qualified Data.Pool as Pool
--import qualified Katip
--import Logger (defaultLogEnv)
--import Models (doMigrations)
import Models
--import Network.Wai.Handler.Warp (run)
--import Safe (readMay)

-- | An action that creates a WAI 'Application' together with its resources,
--   runs it, and tears it down on exit
--runAppDevel :: IO ()
--runAppDevel = do
--    runStderrLoggingT $ withPostgresqlPool dbConStr 10 $ \pool -> liftIO $ do
--        flip runSqlPersistMPool pool $ do
--          runMigration migrateAll
--
--
--dbConStr :: ConnectionString
--dbConStr = "host=192.168.0.100 port=5432 user=inventory_user dbname=inventory_repair_db password=inventory_password"

--makePool :: IO ConnectionPool
--makePool = runKatipT env (createPostgresqlPool (connStr "-test") (envPool Test))
