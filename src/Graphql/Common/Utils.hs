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

module Graphql.Common.Utils where

import RIO
import Prelude (read)
import RIO.List.Partial ((!!))
import System.Random
import Data.Text (Text, pack, strip, splitOn, unpack)
import Data.Morpheus.Types (GQLType(..))
import Data.Time

-- data Pageable = Pageable { pageIndex :: Int, pageSize :: Int } deriving (Generic)

data PageInfo = PageInfo { hasNext:: Bool
                         , hasPreview:: Bool
                         , pageSize :: Int
                         , pageIndex :: Int
                         } deriving (Generic, GQLType)

--data Sort = Sort { isUnsorted :: Bool
--                 , isSorted :: Bool
--                 , direction :: Text
--                 } deriving (Generic, GQLType)

data Page a = Page { totalCount :: Int
                   , content :: [a]
                   , pageInfo :: PageInfo
--                   , sort :: Sort
                   } deriving (Generic, GQLType)

data EntityArg a = EntityArg { arg :: a } deriving (Generic)

data EntityChangeStatusArg = EntityChangeStatusArg { entityIds :: [Int]
                                                   , status :: Text
                                                   } deriving (Generic, GQLType)

data PageArg = PageArg { searchString :: Maybe Text
                       , pageIndex :: Maybe Int
                       , pageSize :: Maybe Int
                       , filters :: Maybe [Predicate]
                       } deriving (Generic, GQLType)

data EntityIdArg = EntityIdArg { entityId :: Int } deriving (Generic, GQLType)

data EntityIdsArg = EntityIdsArg { entityIds :: [Int] } deriving (Generic)

data Predicate = Predicate { field :: Text
                           , operator :: Text
                           , value :: Text
                           , union :: Maybe [Predicate]
                           , conjunction :: Maybe [Predicate]
                           } deriving (Generic, GQLType)


localDay :: IO Day
localDay = fmap utctDay getCurrentTime


randomAlphaNumText :: Int -> IO Text
randomAlphaNumText n = do
                        randomString <- genRandomAlphaNumString n
                        return $ pack randomString

genRandomAlphaNumString :: Int -> IO String
genRandomAlphaNumString 0 = return []
genRandomAlphaNumString n = do
                             let s = ['0'..'9'] ++ ['A'..'Z']
                             r <- randomRIO (0, (length s) - 1)
                             let c = s !! r
                             s <- genRandomAlphaNumString (n - 1)
                             return (c:s)

parseToInteger :: Text -> Int
parseToInteger str = read $ unpack str :: Int

--textToList :: Text -> [Text]
textToList "" _ = []
textToList text f | strip text == "" = []
                  | otherwise = map  (\e -> f $ strip e) (splitOn "," text)

fromText _ "" = []
fromText f text | strip text == "" = []
                | otherwise = map  (\e -> f $ strip e) (splitOn "," text)

--getOperator "=" = (==.)
--getOperator ">" = (>.)
--getOperator ">=" = (>=.)
--getOperator "<=" = (<=.)
--getOperator "<" = (<.)
--
--into field val = Filter field (Left val) (BackendSpecificFilter "in")
--like field val = Filter field (Left $ T.concat ["%", val, "%"]) (BackendSpecificFilter "like")
--
--conjunctionFilters xs = P.concat xs
--unionFilters (x:xs) = foldl (||.) x xs

--getOperator "=" = (E.==.)
--getOperator "!=" = (E.!=.)
--getOperator ">" = (E.>.)
--getOperator ">=" = (E.>=.)
--getOperator "<=" = (E.<=.)
--getOperator "<" = (E.<.)

--conjunctionFilters (x:xs) = foldl (E.&&.) x xs
--unionFilters (x:xs) = foldl (E.||.) x xs