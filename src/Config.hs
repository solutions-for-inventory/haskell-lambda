{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (getDBConfig, DBConfig(..)) where

import GHC.Generics
--import Data.Aeson
import Data.Yaml (decodeFile, FromJSON(..), ToJSON(..))
import Data.Yaml.Config (loadYamlSettings, useEnv)
import Database.Persist.Postgresql (withPostgresqlPool, ConnectionString)
import Text.Printf (printf)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)

data AppConfig = AppConfig { db :: DBConfig
                           } deriving (Generic, Read, Show)

data DBConfig = DBConfig { user :: String
                         , password :: String
                         , host :: String
                         , port :: Int
                         , database :: String
                         , poolsize :: Int
                         } deriving (Generic, Read, Show)

instance FromJSON AppConfig
instance FromJSON DBConfig

getAppConfig :: IO AppConfig
getAppConfig = loadYamlSettings ["config/settings.yml"] [] useEnv :: IO AppConfig

getDBConfig :: IO DBConfig
getDBConfig = do
               appConfig <- getAppConfig
               return $ db appConfig