{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -w #-}

module Graphql.Root (api, apiDoc, apiRaw) where

import Control.Monad.Trans.Class (lift)
import Data.Bits (Bits (clearBit))
import Data.Data (typeOf)
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (gqlDocument)
import Data.Morpheus.Server (printSchema)
import Data.Morpheus.Types (GQLRequest, GQLResponse, GQLType (..), MUTATION, QUERY, ResolverQ, RootResolver (..), Undefined (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import qualified Data.ByteString.Lazy as B
import Graphql.Types.Gql
import RIO

resolveQuery =
  Query
  --                         categories = listCategoryResolver
  --                       , units = listUnitResolver ()
  --                       , inventories = inventoryResolver ()
  --                       , items = itemResolver ()
  --                       , inventoryItems = inventoryItemsResolver ()
    {
    }

{- | The mutation resolver
resolveMutation::Mutation (MUTATION () Handler)
-}
resolveMutation =
  Mutation
  --                             saveCategory = saveCategoryResolver
  --                           , saveUnit = saveUnitResolver
  --                           , inventories = inventoryResolver ()
  --                           , items = itemResolver ()
  --                           , inventoryItems = inventoryItemsResolver ()
    {
    }

rootResolver :: RootResolver IO () Query Mutation Undefined
rootResolver =
  RootResolver
    { queryResolver = resolveQuery
    , mutationResolver = resolveMutation
    --                               , subscriptionResolver = Undefined
    }

-- | Compose the graphQL api
api :: GQLRequest -> IO GQLResponse
api request = interpreter rootResolver request

apiRaw :: ByteString -> IO ByteString
apiRaw = interpreter rootResolver

proxy :: Proxy (RootResolver IO () Query Mutation Undefined)
proxy = Proxy

apiDoc :: B.ByteString
apiDoc = printSchema $ proxy