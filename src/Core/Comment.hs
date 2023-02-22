{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Core.Comment where

import RIO
import Rel8
import Data.Aeson
import Data.Time
import Data.UUID

import Core.User
import Core.Article

data Comment = Comment
    { commentId          :: CommentId
    , commentUUID        :: UUID
    , commentBody        :: Text
    , commentArticleId   :: ArticleId
    , commentAuthorId    :: UserId
    , commentCreatedAt   :: UTCTime
    , commentUpdatedAt   :: UTCTime
    }

newtype CommentId = CommentId { getCommentId :: Int64 }
    deriving newtype (Eq, Show, Read, FromJSON, ToJSON, DBEq, DBType)
