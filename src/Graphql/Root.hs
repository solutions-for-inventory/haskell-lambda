{-# OPTIONS_GHC -w #-}
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
{-# LANGUAGE NoImplicitPrelude     #-}


module Graphql.Root (api, apiDoc, apiRaw) where

import RIO
import Data.Text (Text, pack)
import Control.Monad.Trans.Class (lift)
import Data.Proxy (Proxy(..))
import Data.Morpheus              (interpreter)
import Data.Morpheus.Server (printSchema)
import Data.Morpheus.Types (RootResolver (..), GQLType(..), Undefined(..), QUERY, MUTATION, GQLRequest, GQLResponse, ResolverQ)
import Data.Morpheus.Document (gqlDocument)
import Data.Morpheus.Server (printSchema)
import Graphql.Types.Gql

resolveQuery = Query {
--                         categories = listCategoryResolver
--                       , units = listUnitResolver ()
--                       , inventories = inventoryResolver ()
--                       , items = itemResolver ()
--                       , inventoryItems = inventoryItemsResolver ()
                       }
-- | The mutation resolver
--resolveMutation::Mutation (MUTATION () Handler)
resolveMutation = Mutation {
--                             saveCategory = saveCategoryResolver
--                           , saveUnit = saveUnitResolver
--                           , inventories = inventoryResolver ()
--                           , items = itemResolver ()
--                           , inventoryItems = inventoryItemsResolver ()
                           }

rootResolver :: RootResolver IO () Query Mutation Undefined
rootResolver = RootResolver {
                                 queryResolver = resolveQuery
                               , mutationResolver = resolveMutation
--                               , subscriptionResolver = Undefined
                             }

-- | Compose the graphQL api
api:: GQLRequest -> IO GQLResponse
api request = interpreter rootResolver request

apiRaw :: ByteString -> IO ByteString
apiRaw = interpreter rootResolver

proxy :: Proxy (RootResolver IO () Query Mutation Undefined)
proxy = Proxy

apiDoc = printSchema $ proxy
