{-# LANGUAGE RankNTypes #-}
module Db.Transaction where

import RIO
import Data.List
import Hasql.Connection (Connection)
import Hasql.Pool ( Pool, UsageError(..), acquire )
import qualified Hasql.Pool as Pool
import Hasql.Transaction ( Transaction, condemn, statement, sql )
import qualified Hasql.Session as Session
import qualified Hasql.Transaction.Sessions as Hasql
import Hasql.Statement (Statement)

import Config
--import Conduit.App
--import Environment

loadPool :: ByteString -> Int -> IO Pool
loadPool connectString poolSize = acquire poolSize (Just 1) connectString

runTransactionWithConnection :: MonadIO m => Connection -> Transaction b -> m b
runTransactionWithConnection conn transaction = do
    e <- liftIO $ Session.run (Hasql.transaction Hasql.Serializable Hasql.Write transaction) conn
    either throwIO pure e

runTransactionWithPool :: MonadIO m => Pool -> Transaction b -> m b
runTransactionWithPool pool transaction = do
    result <- liftIO $ Pool.use pool (Hasql.transaction Hasql.Serializable Hasql.Write transaction)
    case result of
        Right e -> pure e
        Left (ConnectionUsageError e) -> error $ "Failed to connect to database, error: " ++ show e
        Left (SessionUsageError e) -> throwIO e

runStmt :: Statement () a -> Transaction a
runStmt = statement ()

--runTransaction :: forall a m env . (MonadReader env m, MonadIO m) => Transaction a -> m a
runTransaction transaction = do
    pool <- loadPool "host=192.168.0.100 port=5432 user=user dbname=test password=123" 10
    runTransactionWithPool pool transaction

executeStmt :: Statement () a -> IO a
executeStmt = runTransaction . runStmt

truncateTables :: [Text] -> Transaction ()
truncateTables tables = sql $ mconcat
    [ "TRUNCATE "
    , fromString $ intercalate ", " (map show tables)
    ," RESTART IDENTITY;"
    ]