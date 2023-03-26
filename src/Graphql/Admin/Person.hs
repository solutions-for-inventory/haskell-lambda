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

module Graphql.Admin.Person (
                  personResolver
--                , getPersonByIdResolver
                ) where

--import Models
import Control.Monad.Trans.Class (lift)
--import Database.Persist.Sql (toSqlKey, fromSqlKey)
--import Database.Persist.Postgresql(getJustEntity, get, toSqlKey, selectList, (==.), ToBackendKey, SqlBackend)
--import qualified Database.Esqueleto      as E
--import Database.Esqueleto ((==.))
--import Database.Esqueleto      ((^.), (%), (++.){-, (?.), notIn, in_-})
--import Prelude as P hiding (zip)
import RIO
import Rel8
import Db
import Core.User
import Graphql.Utils
import Graphql.Admin.DataTypes
import Control.Monad.Trans.Class (MonadTrans)
import Data.Typeable
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (runReaderT, ReaderT)
import Data.Maybe (fromJust)
import Data.Morpheus.Types (ResolverQ)
-- Query Resolvers
--type App m a = ReaderT SqlBackend m a
--personResolver :: (Applicative f, Typeable o, MonadTrans (o ())) => () -> f (Persons o)
--personResolver :: (Applicative f, Typeable o, MonadTrans (o ())) => () -> f (Persons o)
personResolver :: () -> ResolverQ () IO Persons
personResolver _ = pure Persons {
                                 person = getPersonByIdResolver_
--                                , page = pagePersonResolver
--                                , createUpdatePerson = createUpdatePersonResolver
                                }

--getPersonByIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => EntityIdArg -> o () IO Person
--getPersonByIdResolver_ :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => EntityIdArg -> o () IO Person
--getPersonByIdResolver_ :: forall (t :: (* -> *) -> * -> *)(m :: * -> *).(MonadTrans t, MonadUnliftIO m) => EntityIdArg -> t m Person
getPersonByIdResolver_ :: EntityIdArg -> ResolverQ () IO Person
getPersonByIdResolver_ EntityIdArg {..} = lift  $ do
--                                          let personEntityId = Person_Key $ fromIntegral entityId
--                                           persons <- runDB $ selectList [Person_LastName ==. "personId"] []
--                                          person <- runDB $ getJustEntity personEntityId
                                          users <- executeStmt $ select $ getUserByIdStmt (litExpr $ UserId $ fromIntegral entityId)
                                          let user = fromJust  $ listToMaybe $ map mapUserEntityToUser users
                                          return $ toPersonQL user



--getPersonByIdResolver :: forall (o :: * -> (* -> *) -> * -> *).(Typeable o, MonadTrans (o ())) => Person_Id -> () -> o () Handler (Person o)
--getPersonByIdResolver personId _ = lift $ do
--                                      person <- runDB $ getJustEntity personId
--                                      return $ toPersonQL

--pagePersonResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => PageArg -> t Handler (Page (Person o))
--pagePersonResolver page = lift $ do
--                                countItems <- personQueryCount page
--                                persons <- personQuery page
--                                let personsQL = P.map (\p -> toPersonQL p) persons
--                                return Page { totalCount = countItems
--                                            , content = personsQL
--                                            , pageInfo = PageInfo { hasNext = (pageIndex' * pageSize' + pageSize' < countItems)
--                                                                  , hasPreview = pageIndex' * pageSize' > 0
--                                                                  , pageSize = pageSize'
--                                                                  , pageIndex = pageIndex'
--                                            }
--                                }
--                         where
--                          PageArg {..} = page
--                          pageIndex' = case pageIndex of Just  x  -> x; Nothing -> 0
--                          pageSize' = case pageSize of Just y -> y; Nothing -> 10

--personFilters :: Monad m => E.SqlExpr (Entity Person_) -> PageArg -> m (E.SqlExpr (E.Value Bool))
--personFilters person PageArg {..} = do
--                            let searchFilters = case searchString of
--                                                  Just s -> [ person ^. Person_DocumentId E.==. E.val s
--                                                            , person ^. Person_FirstName `E.ilike` (%) ++. E.val s ++. (%)
--                                                            , person ^. Person_LastName `E.ilike` (%) ++. E.val s ++. (%)
--                                                            ]
--                                                  Nothing -> [person ^. Person_Id E.==. person ^. Person_Id]
--                            let searchFilters' = unionFilters searchFilters
--                            return searchFilters'
--
--personQueryCount :: PageArg -> Handler Int
--personQueryCount page =  do
--                      result  <- runDB
--                                   $ E.select
--                                   $ E.from $ \ person -> do
--                                        filters <- personFilters person page
--                                        E.where_ filters
--                                        return E.countRows
--                      return $ fromMaybe 0 $ listToMaybe $ fmap (\(E.Value v) -> v) $ result
--
--personQuery :: PageArg -> Handler [Entity Person_]
--personQuery page =  do
--                      result <- runDB
--                                   $ E.select
--                                   $ E.from $ \ person -> do
--                                        pFilters <- personFilters person page
--                                        E.where_ pFilters
--                                        E.offset $ pageIndex_ * pageSize_
--                                        E.limit pageSize_
--                                        return person
--                      return result
--                      where
--                        PageArg {..} = page
--                        pageIndex_ = fromIntegral $ case pageIndex of Just  x  -> x; Nothing -> 0
--                        pageSize_ = fromIntegral $ case pageSize of Just y -> y; Nothing -> 10

--createUpdatePersonResolver :: (Typeable o, MonadTrans t, MonadTrans (o ())) => PersonArg -> t Handler (Person o)
--createUpdatePersonResolver arg = lift $ do
--                                personId <- createOrUpdatePerson arg
--                                person <- runDB $ getJustEntity personId
--                                return $ toPersonQL person

--createOrUpdatePerson :: PersonArg -> Handler Person_Id
--createOrUpdatePerson personArg = do
--                               let PersonArg{..} = personArg
--                               now <- liftIO getCurrentTime
--                               personEntityId <- if personId > 0 then
--                                            do
--                                              let personKey = (toSqlKey $ fromIntegral personId)::Person_Id
--                                              _ <- runDB $ update personKey [  Person_FirstName =. firstName
--                                                                             , Person_LastName =. lastName
--                                                                             , Person_DocumentType =. documentType
--                                                                             , Person_DocumentId =. documentId
--                                                                             , Person_ModifiedDate =. Just now
--                                                                            ]
--                                              return personKey
--                                            else
--                                              do
--                                                personKey <- runDB $ insert (fromPersonQL_ personArg now Nothing)
--                                                return personKey
--                               _ <- case address of
--                                      Nothing -> return ()
--                                      Just addressArg ->  do
--                                                          _ <- createOrUpdateAddress personEntityId addressArg
--                                                          return ()
--                               _ <- createOrUpdateContactInfo personEntityId contactInfo
--                               return personEntityId

--toPersonQL :: Entity Person_ -> (Person Res)
--toPersonQL :: (Typeable o, MonadTrans (o ())) => Entity Person_ -> Person o
--toPersonQL = Person { personId = 1233
--                    , firstName = "person_FirstName"
--                    , lastName = "person_LastName"
--                    , documentType = "person_DocumentType"
--                    , documentId = "321"
--                    , createdDate = "dummy createdDate"
--                    , modifiedDate = Nothing
--                    }
toPersonQL User{..} = Person { personId = fromIntegral $ getUserId userId
                             , firstName = userBio
                             , lastName = "person_LastName"
                             , documentType = "person_DocumentType"
                             , documentId = "321"
                             , createdDate = "dummy createdDate"
                             , modifiedDate = Nothing
                             }
--                                 where
--                                  Person_ {..} = person
--                                  md = case person_ModifiedDate of
--                                        Just d -> Just $ fromString $ show d
--                                        Nothing -> Nothing

--fromPersonQL_ :: PersonArg -> UTCTime -> Maybe UTCTime -> Person_
--fromPersonQL_ PersonArg {..} cd md = Person_ { person_FirstName = firstName
--                                             , person_LastName = lastName
--                                             , person_DocumentType = documentType
--                                             , person_DocumentId = documentId
--                                             , person_OrgUnitId = (toSqlKey $ fromIntegral $ orgUnitId)
--                                             , person_CreatedDate = cd
--                                             , person_ModifiedDate =  md
--                                             }


{-

query {
  persons {
    person(entityId: 16) {
    personId
    firstName
    lastName
    createdDate
    modifiedDate
    address {
      addressId
      city
      country
      state
    }

    contactInfo {
      contactId
      contact
      contactType
    }
    }
  }
}


mutation {
  savePerson(personId:0, firstName: "test", lastName: "sss", documentType: "sss", documentId: "0") {
    personId
    firstName
    lastName
    createdDate
    modifiedDate
    address(addressId: 0, street1: "street1", street2: "street2", street3: "street1", zip:"ss", city: "OR", state: "s", country:"ssss") {
      addressId
      city
      country
      state
    }

    contactInfo(contactInfo: [{contactId: 0, contact: "mss", contactType: "mail"}]) {
      contactId
      contact
      contactType
    }
  }
}
-}
