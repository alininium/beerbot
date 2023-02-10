{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import DataTypes
    ( userFromTuple,
      Action(..),
      Model,
      Pronoun(Unset),
      User(..),
      userToTuple )
import Actions ( commandToAction, messageToCommand )

import           Data.Text                        (pack, unpack, Text)
import Data.Maybe ( fromJust, fromMaybe, isJust )
import Debug.Trace ()

import           System.Environment.MrEnv (envAsString)
import           Configuration.Dotenv (loadFile, defaultConfig)
import Telegram.Bot.API
    ( defaultTelegramClientEnv,
      Update(updateMessage),
      Token(..),
      ParseMode(Markdown) )
import Telegram.Bot.Simple
    ( startBot_, (<#), reply, BotApp(..), Eff, ReplyMessage(..) )

import Control.Monad.Trans (liftIO)
import Control.Monad (when, void)

import Network.HTTP
    ( getResponseBody,
      simpleHTTP,
      setRequestBody,
      Request(Request, rqURI, rqMethod, rqHeaders, rqBody),
      RequestMethod(GET),
      Request_String )
import Network.URI ( URI, parseURI )
import Data.ByteString.Lazy.UTF8 as BLU ( fromString )
import Codec.Binary.UTF8.String (utf8Encode)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple
    ( execute, connectPostgreSQL, Only(Only) )
import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, decode )
import qualified Database.PostgreSQL.Simple as Database
import GHC.Integer (eqInteger)
import qualified Network.HTTP.Stream as Database

data AnalyzeResult = AnalyzeResult {
  response :: PronounsUsed
} deriving (Generic, Show)

data PronounsUsed = PronounsUsed {
  fem :: Integer,
  masc :: Integer,
  plur :: Integer
} deriving (Generic, Show)

instance FromJSON AnalyzeResult
instance FromJSON PronounsUsed

echoBot :: BotApp Model Action
echoBot = BotApp
  { botInitialModel = "q"
  , botAction = updateToAction
  , botHandler = handleAction
  , botJobs = []
  }


constructReply :: String -> ReplyMessage
constructReply s = ReplyMessage
          { replyMessageText = pack s
          , replyMessageParseMode = Just Markdown
          , replyMessageEntities = Nothing
          , replyMessageDisableWebPagePreview = Nothing
          , replyMessageDisableNotification = Nothing
          , replyMessageProtectContent = Nothing
          , replyMessageReplyToMessageId = Nothing
          , replyMessageAllowSendingWithoutReply = Nothing
          , replyMessageReplyMarkup = Nothing
          }

updateToAction :: Update -> Model -> Maybe Action
updateToAction update _
  | isJust $ updateMessage update = do
    message         <- updateMessage update
    command         <- messageToCommand message
    commandToAction message command

  | otherwise = Nothing


--pipe <- liftIO $ DB.connect (DB.host "127.0.0.1")
--e <- trace "connecting" $ liftIO $ DB.access pipe DB.master "pivobot" run
--DB.close pipe

formJSON :: Text -> String
formJSON t = utf8Encode $ "{\"string\": \"" ++ unpack t ++ "\"}"

request :: Text -> URI -> Request_String
request t uri = setRequestBody (Request{ rqURI = uri,
                         rqMethod = GET,
                         rqHeaders = [],
                         rqBody = "" }) ("application/json", formJSON t)

-- handleE :: Monad m => (ConnError -> m a) -> Either ConnError a -> m a
-- handleE h (Left e) = h e
-- handleE _ (Right v) = return v

updateUserPronounsUsed :: DataTypes.User -> PronounsUsed -> DataTypes.User
updateUserPronounsUsed u p = TelegramUser {
                          telegram_id = telegram_id u,
                          username    = username u,
                          pronoun     = pronoun u,
                          used_mask   = used_mask u + if masc p > fem p then 1 else 0,
                          used_fem    = used_fem  u + if masc p < fem p then 1 else 0,
                          used_plur   = used_plur u + if (masc p == 0 && fem p == 0) && (plur p > 0) then 1 else 0
}

analyzeGender :: Text -> IO PronounsUsed
analyzeGender text = do
    ersp <- Network.HTTP.simpleHTTP (request text . fromJust $ parseURI "http://localhost:8000/analyze")
    a <- BLU.fromString <$> getResponseBody ersp
    let resp = response <$> (Data.Aeson.decode a :: Maybe AnalyzeResult)
    return $ fromMaybe PronounsUsed {masc = 0, fem = 0, plur = 0} resp


handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  ReplyText text -> model <# do
    reply $ constructReply text
  NoAction -> model <# (do
    return ())

  AnalyzeText text userName userId -> model <# liftIO (do
    genderUsage <- analyzeGender text
    conn <- connectPostgreSQL "postgres://postgres:postgres@127.0.0.1:5432"
    user <- getUser conn userId userName
    dbUpdateUser conn (updateUserPronounsUsed user genderUsage)
    return ())

-- Provides consistency with the database, creating a new user if nessesary 
getUser :: Database.Connection -> Integer -> Text -> IO DataTypes.User
getUser conn userId userName = do
  users <- dbGetUser conn userId
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
      dbInsertUser conn newUser
      return newUser
    else return $ head users


dbGetUser :: Database.Connection -> Integer -> IO [DataTypes.User]
dbGetUser conn userId = map userFromTuple <$> Database.query conn "select telegram_id,username,pronoun,used_mask,used_fem,used_plur from users where telegram_id = ?" (Only userId)

dbInsertUser :: Database.Connection -> DataTypes.User -> IO ()
dbInsertUser conn u = void $ execute conn "insert into users (username,pronoun,used_mask,used_fem,used_plur, telegram_id) values (?,?,?,?,?,?)" (userToTuple u)

dbUpdateUser :: Database.Connection -> DataTypes.User -> IO ()
dbUpdateUser conn u = void $ execute conn "update users set (username,pronoun,used_mask,used_fem,used_plur) = (?,?,?,?,?) where users.telegram_id = ?" (userToTuple u)

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ echoBot env

main :: IO ()
main = do
  _ <- loadFile defaultConfig
  token <- Token . pack <$> envAsString "TOKEN" "none"
  putStrLn "Starting bot"
  run token
