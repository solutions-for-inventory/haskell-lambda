{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Graphql.Types.Gql where

import Data.Morpheus.Types (GQLType)
import Graphql.Common.Utils (Page, PageArg, EntityIdArg, EntityChangeStatusArg)
import RIO


-- ABSTRACT TYPES
data Query m = Query {
                            categories :: PartCategoryFilter -> m [PartCategory]
                          , units :: m [Unit]
                          , inventories :: m (Inventories m)
                          , items :: m (Items m)
                          , inventoryItems :: m (InventoryItems m)
                         } deriving (Generic, GQLType)

data Mutation m = Mutation {
                             saveCategory :: PartCategoryArg -> m PartCategory
                           , saveUnit :: UnitArg -> m Unit
                           , inventoryItems :: m (InventoryItems m)
                           , items :: m (Items m)
                           , inventories :: m (Inventories m)
                           } deriving (Generic, GQLType)


-- BUSINESS TYPES
data Inventory m = Inventory { inventoryId :: Int
                             , name :: Text
                             , description :: Text
                             , allowNegativeStocks :: Bool
                             , status :: Text
                             , inventoryItems :: PageArg -> m (Page (InventoryItem m))
                             , availableItems :: PageArg -> m (Page (Part m))
                             , createdDate :: Text
                             , modifiedDate :: Maybe Text
                             } deriving (Generic, GQLType)

data Inventories m = Inventories { inventory :: EntityIdArg ->  m (Inventory m)
                                 , list ::  m [Inventory m]
                                 , saveInventory :: InventoryArg -> m (Inventory m)
                                 , saveInventoryItems :: InventoryItemsArg -> m [InventoryItem m]
                                 } deriving (Generic, GQLType)

-- Mutation
data InventoryArg = InventoryArg { inventoryId :: Int
                                 , name :: Text
                                 , description :: Text
                                 , allowNegativeStocks :: Bool
                                 , status :: Text
                                 , orgUnitId :: Int
                                 } deriving (Generic, GQLType)

data InventoryItem m = InventoryItem { inventoryItemId :: Int
                                     , status :: Text
                                     , level :: Int
                                     , maxLevelAllowed :: Int
                                     , minLevelAllowed :: Int
                                     , price :: Float
                                     , location :: Text
                                     , dateExpiry :: Maybe Text
                                     , inventory ::  m (Inventory m)
                                     , item ::  m (Part m)
                                     , createdDate :: Text
                                     , modifiedDate :: Maybe Text
                                     } deriving (Generic, GQLType)

data InventoryItems m = InventoryItems { inventoryItem :: EntityIdArg -> m (InventoryItem m)
                                       , page :: PageArg -> m (Page (InventoryItem m))
                                       , saveInventoryItem :: InventoryPartArg -> m (InventoryItem m)
                                       } deriving (Generic, GQLType)

data InventoryPartArg = InventoryPartArg { inventoryItemId :: Int
                                         , status :: Text
                                         , level :: Int
                                         , code :: Text
                                         , maxLevelAllowed :: Int
                                         , minLevelAllowed :: Int
                                         , price :: Float
                                         , location :: Text
                                         , inventoryId :: Int
                                         , dateExpiry :: Maybe Text
                                         , partId :: Int
                                         } deriving (Generic, GQLType)

data InventoryItemsArg = InventoryItemsArg { level :: Int
                                           , maxLevelAllowed :: Int
                                           , minLevelAllowed :: Int
                                           , price :: Float
                                           , location :: Text
                                           , dateExpiry :: Maybe Text
                                           , inventoryId :: Int
                                           , itemIds :: [Int]
                                           } deriving (Generic, GQLType)

data Part m = Part { partId :: Int
                   , partNumber :: Text
                   , name :: Text
                   , defaultPrice :: Float
                   , description :: Maybe Text
                   , manufacturer :: Maybe Text
                   , model :: Maybe Text
                   , itemType :: Text
                   , notes:: Maybe Text
                   , status :: Text
                   , images :: [Text]
                   , partCategory :: Maybe(m PartCategory)
                   , unit :: Maybe (m Unit)
                   , inventoryItems :: PageArg -> m (Page (InventoryItem m))
                   , createdDate :: Text
                   , modifiedDate :: Maybe Text
                   } deriving (Generic, GQLType)

data Items m = Items { item :: EntityIdArg -> m (Part m)
                     , page :: PageArg -> m (Page (Part m))
                     , saveItem :: PartArg -> m (Part m)
                     , changeItemStatus :: EntityChangeStatusArg -> m Bool
                     } deriving (Generic, GQLType)

data PartArg = PartArg { partId :: Int
                       , partNumber :: Text
                       , name :: Text
                       , defaultPrice :: Float
                       , description :: Maybe Text
                       , manufacturer :: Maybe Text
                       , model :: Maybe Text
                       , itemType :: Text
                       , notes :: Maybe Text
                       , status :: Text
                       , images :: [Text]
                       , categoryId :: Maybe Int
                       , unitId :: Maybe Int
                       , orgUnitId :: Int
                       } deriving (Generic, GQLType)


data Unit = Unit { unitId :: Int
                 , key :: Text
                 , label :: Text
                 , createdDate :: Text
                 , modifiedDate :: Maybe Text
                 } deriving (Generic, GQLType)

data UnitArg = UnitArg { unitId :: Int
                       , key :: Text
                       , label :: Text
                       } deriving (Generic, GQLType)


data PartCategory = PartCategory { categoryId :: Int
                         , name :: Text
                         , key :: Maybe Text
                         , scope :: Text
                         , description :: Text
                         , orgUnitId :: Int
                         , createdDate :: Text
                         , modifiedDate :: Maybe Text
                         } deriving (Generic, GQLType)

-- Mutation
data PartCategoryArg = PartCategoryArg { categoryId :: Int
                                       , name :: Text
                                       , key :: Maybe Text
                                       , scope :: Text
                                       , description :: Text
                                       , orgUnitId :: Int
                                       } deriving (Generic, GQLType)

data PartCategoryFilter = PartCategoryFilter { orgUnitId :: Maybe Int, scope :: Maybe Text } deriving (Generic, GQLType)
{-
query {
  items {
    page(pageIndex:0, pageSize: 10) {
      totalCount
      pageInfo {
        pageIndex
        pageSize
        hasNext
        hasPreview
      }
      content {
        partId
        name
        unit
        defaultPrice
        description
        code
        images
        createdDate
        modifiedDate
        category {
          categoryId
          name
        }
      }

    }
  }
}

mutation {
  saveRole(partId:10, key: "test12", name: "sloss", description: "option" active: true) {
    partId
    key
    description
    active
    createdDate
    modifiedDate
    privileges(entityIds: [16]) {
      privilegeId
      key
      description
      active
      createdDate
      modifiedDate
    }
  }
}

-}