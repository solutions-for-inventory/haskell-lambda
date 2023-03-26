{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
module Repository.User where

import RIO
import Rel8

--import App
import Core.User
import Core.Password
import Db

getUserById :: UserId -> IO (Maybe User)
getUserById uid = do
    users <- executeStmt $ select $ getUserByIdStmt (litExpr uid)
    return $ listToMaybe $ map mapUserEntityToUser users

getUserByName :: Username -> IO (Maybe User)
getUserByName username = do
    users <- executeStmt $ select $ getUserByNameStmt username
    return $ listToMaybe $ map mapUserEntityToUser users

getUserByEmail :: EmailAddress -> IO (Maybe User)
getUserByEmail email = do
    users <- executeStmt $ select $ getUserByEmailStmt email
    return $ listToMaybe $ map mapUserEntityToUser users

getUserByEmailAndPassword :: EmailAddress -> Password  -> IO (Maybe User)
getUserByEmailAndPassword email password = do
    users <- executeStmt $ select $ getUserByEmailStmt email
    return $ verifyPassword' =<< listToMaybe users
    where
        verifyPassword' user =
            if verifyPassword password (entityUserPassword user)
            then
                Just $ mapUserEntityToUser user
            else
                Nothing

saveNewUser :: User -> Password -> IO (Maybe User)
saveNewUser user password = do
    hashedPwdAndSalt <- liftIO $ hashPassword password
    userIds <- executeStmt $ insert $ insertUserStmt user hashedPwdAndSalt
    return $ listToMaybe userIds >>= \uid -> Just $ user { userId = uid }

updateUser :: User -> Maybe Password -> IO Bool
updateUser user mbPassword = do
    hashAndSalt <-
        case mbPassword of
            Just password -> liftIO $ Just <$> hashPassword password
            _             -> return Nothing
    rows <- executeStmt $ update $ updateUserStmt user hashAndSalt
    return $ rows > 0

checkFollowship :: User -> UserId -> IO Bool
checkFollowship user following = do
    exists <- executeStmt $ select $ checkFollowshipStmt user (litExpr following)
    return $ exists == [True]

followUser :: User -> UserId -> IO Bool
followUser user toFollow = do
    rows <-executeStmt $ insert $ createFollowshipStmt (userId user) toFollow
    return $ rows == 1

unfollowUser :: User -> UserId -> IO Bool
unfollowUser user following = do
    rows <- executeStmt $ delete $ removeFollowshipStmt (userId user) following
    return $ rows == 1
