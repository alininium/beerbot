{-# LANGUAGE OverloadedStrings #-}
module DB (
    updateUserWithIO,
    getUser,
    iUpdateUser
) where

import Database.PostgreSQL.Simple
    ( execute, connectPostgreSQL, Only(Only) )
import qualified Database.PostgreSQL.Simple as Database
import Control.Monad (void)

import DataTypes



getConn_ :: IO Database.Connection
getConn_ = connectPostgreSQL "postgres://postgres:postgres@127.0.0.1:5432"

getUser_ :: Database.Connection -> Integer -> IO [DataTypes.User]
getUser_ conn userId = map userFromTuple <$> Database.query conn "select telegram_id,username,pronoun,used_mask,used_fem,used_plur from users where telegram_id = ?" (Only userId)

insertUser_ :: Database.Connection -> DataTypes.User -> IO ()
insertUser_ conn u = void $ execute conn "insert into users (username,pronoun,used_mask,used_fem,used_plur, telegram_id) values (?,?,?,?,?,?)" (userToTuple u)

updateUser_ :: Database.Connection -> DataTypes.User -> IO ()
updateUser_ conn u = void $ execute conn "update users set (username,pronoun,used_mask,used_fem,used_plur) = (?,?,?,?,?) where users.telegram_id = ?" (userToTuple u)

updateUserWithIO :: UserFromMessage -> (User -> IO User) -> IO ()
updateUserWithIO user f = do
    conn <- getConn_
    fetchedUser <- getUser_ conn (from_message_telegram_id user) 
    if (not . null) fetchedUser 
        then f (head fetchedUser) >>= updateUser_ conn
        else return ()

getUser :: UserFromMessage -> IO [DataTypes.User]
getUser user = getConn_ >>= (`getUser_` (from_message_telegram_id user))

-- Provides consistency with the database, creating a new user if nessesary 
iUpdateUser :: UserFromMessage -> IO DataTypes.User
iUpdateUser UserFromMessage {from_message_telegram_id = userId,
                             from_message_username    = userName} = do
  conn <- getConn_ 
  users <- getUser_ conn userId
  if null users
    then do
      -- If no user with such id exists in db create him
      let newUser = TelegramUser {
                telegram_id = userId,
                username    = userName,
                pronoun     = Unset,
                used_mask   = 0,
                used_fem    = 0,
                used_plur   = 0
      }
      insertUser_ conn newUser
      return newUser
    else return $ head users