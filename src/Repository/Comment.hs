{-# LANGUAGE RankNTypes #-}
module Repository.Comment where

import RIO
import Rel8
import Data.UUID

--import App
import Core.User
import Core.Article
import Core.Comment
import Db

type EnrichedComment = (Comment, User, Bool )

getEnrichedCommentsByArticleId :: Maybe User -> ArticleId -> IO [EnrichedComment]
getEnrichedCommentsByArticleId mbUser articleId = do
    results <- executeStmt $ select $ do
        comment <- getCommentsByArticleIdStmt (litExpr articleId)
        author <- getUserByIdStmt (entityCommentAuthorId comment)
        followingAuthor <- maybe (return $ litExpr False) (\user -> checkFollowshipStmt user (entityCommentAuthorId comment)) mbUser
        return (comment, author, followingAuthor)
    return $ map (\(comment, author, following) -> (mapCommentEntityToComment comment, mapUserEntityToUser author, following)) results

getCommentById :: CommentId -> IO (Maybe Comment)
getCommentById commentId = do
    comments <- executeStmt $ select $ getCommentByIdStmt (litExpr commentId)
    return $ listToMaybe $ map mapCommentEntityToComment comments

getCommentByUUID :: UUID -> IO (Maybe Comment)
getCommentByUUID  uuid = do
    comments <- executeStmt $ select $ getCommentByUUIDStmt (litExpr uuid)
    return $ listToMaybe $ map mapCommentEntityToComment comments

addComment :: Comment -> IO (Maybe Comment)
addComment comment = do
    commentId <- executeStmt $ insert (insertCommentStmt comment)
    return $ updateCommentId =<< listToMaybe commentId
    where
        updateCommentId commentId' = Just $ comment {commentId = commentId' }

deleteCommentById :: CommentId -> IO Bool
deleteCommentById commentId = do
    changedRows <- executeStmt $ delete $ deleteCommentStmt commentId
    return $ changedRows == 1
