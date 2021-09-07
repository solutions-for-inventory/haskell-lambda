{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}


module Models where

import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH
       (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Database.Persist.Postgresql (runMigration, runSqlPersistMPool, withPostgresqlPool, withPostgresqlConn, runSqlPool, ConnectionString, createPostgresqlPool)
import Control.Monad.Reader (runReaderT)
import Database.Persist (Entity(..))
import Database.Persist (selectList)
import Database.Persist.Postgresql(getJustEntity)
import Data.Pool (Pool)
--import Data.Conduit(MonadUnliftIO)
--import Control.Exception.Safe
--import Say
--import Config (Config, configPool)
import Data.Text (Text)
import Data.Time

share
    [ mkPersist sqlSettings
    , mkMigrate "migrateAll"
    ] [persistLowerCase|

Person_ json sql=t_person
    Id sql=person_id
    firstName Text
    lastName Text
    documentType Text
    documentId Text
    createdDate UTCTime
    modifiedDate UTCTime Maybe
    UniquePersonDocumentId documentId
    deriving Show
|]

connectionString :: ConnectionString
connectionString = "host=192.168.0.100 port=5432 user=inventory_user dbname=inventory_repair_db password=inventory_password"

migrateDB  :: IO ()
migrateDB  = runDB (runMigration migrateAll)

--runDbDev :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDB action = runStdoutLoggingT $ withPostgresqlPool connectionString 10 $ \ pool -> runSqlPool action pool
--runDB action = runStdoutLoggingT $ withPostgresqlConn connectionString $ \ backend -> runReaderT action backend

--listPersons :: Person_Id -> IO (Entity Person_)
--listPersons personId = do
--              person <- runDB $ getJustEntity personId
--              return person

{-
Sample haskell persistence
https://mmhaskell.com/blog/2017/10/2/trouble-with-databases-persevere-with-persistent
-}