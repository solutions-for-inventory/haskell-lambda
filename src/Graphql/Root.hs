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


module Graphql.Root (api, apiDoc) where

import           GHC.Generics
import Data.Text (Text, pack)
import Control.Monad.Trans.Class (lift)
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Document     ()
import           Data.Morpheus.Types        (RootResolver (..), GQLType(..), Undefined(..), Res, MutRes, GQLRequest, GQLResponse)
import           Data.Morpheus.Document (toGraphQLDocument)
import           Data.ByteString.Lazy.Internal (ByteString)
import           Graphql.Utils ()
import           Graphql.Admin.DataTypes
import           Graphql.Admin.Person

data QueryQL m = QueryQL { -- deity :: DeityArgs -> m Deity
--                           persons :: () -> Res () IO (Persons Res)
                           persons :: () -> m (Persons Res)
                         } deriving (Generic, GQLType)

data Mutation m = Mutation { persons :: () -> MutRes () IO (Persons MutRes)
                           } deriving (Generic, GQLType)

--data DeityArgs = DeityArgs { name :: Text, mythology :: Maybe Text } deriving (Generic)

-- | The query resolver
--resolveQuery::QueryQL (Res () IO)
resolveQuery = QueryQL { --deity = resolveDeity
                         persons = personResolver
                       }
-- | The mutation resolver
--resolveMutation::Mutation (MutRes () IO)
resolveMutation = Mutation {
                             persons = personResolver
                           }


-- BASE EXAMPLE
-- https://github.com/dnulnets/haccessability
--dbFetchDeity:: Text -> IO Deity
--dbFetchDeity name = do
--                     let userId = (toSqlKey 3)::User_Id
--                     deity <- runDB $ getEntity userId
--                     return $ Deity {fullName = "dummy", power = Just "Shapeshifting", tests = testsResolver}

--resolveDeity :: DeityArgs -> Res e IO Deity
--resolveDeity DeityArgs { name, mythology } = lift $ dbFetchDeity name

--testsResolver :: TestArg -> Res e IO NoDeity
--testsResolver TestArg {yourFullName } = pure NoDeity {noFullName = "Test no full am", nopower = Just "no power"}

rootResolver :: RootResolver IO () QueryQL Undefined Undefined
rootResolver = RootResolver { queryResolver = resolveQuery
                               , mutationResolver = Undefined
                               , subscriptionResolver = Undefined
                               }

-- | Compose the graphQL api
api:: GQLRequest -> IO GQLResponse
api request = do
               interpreter rootResolver request

apiDoc :: Data.ByteString.Lazy.Internal.ByteString
apiDoc = toGraphQLDocument $ Just rootResolver
