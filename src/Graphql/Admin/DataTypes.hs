{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module Graphql.Admin.DataTypes where

import GHC.Generics
import Data.Text (Text, pack)
import Data.Morpheus.Kind (INPUT_OBJECT)
import Data.Morpheus.Types (GQLType(..))
import Graphql.Utils (Page, PageArg, EntityIdArg, EntityChangeStatusArg)

-- PERSON DATA TYPES
data Person = Person { personId :: Int
                       , firstName :: Text
                       , lastName :: Text
                       , documentType :: Text
                       , documentId :: Text
                       , createdDate :: Text
                       , modifiedDate :: Maybe Text
                       } deriving (Generic, GQLType)

data Persons o = Persons { person :: EntityIdArg -> o () IO Person
                         } deriving (Generic, GQLType)

--data Query m = Query
--  { deity :: DeityArgs -> m Deity
--  } deriving (Generic, GQLType)

-- Person Graphql Arguments
data PersonArg = PersonArg { personId :: Int
                           , firstName :: Text
                           , lastName :: Text
                           , documentType :: Text
                           , documentId :: Text
                           } deriving (Generic)

instance GQLType PersonArg where
    type  KIND PersonArg = INPUT_OBJECT
    description = const $ Just $ pack "The item that holds the person information"