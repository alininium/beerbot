{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import DataTypes
import Actions ( commandToAction, messageToCommand, applyPronouns )

import           Data.Text                        (pack, unpack, Text, append)
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
import Control.Monad (void)

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
import Database.PostgreSQL.Simple
    ( execute, connectPostgreSQL, Only(Only) )
import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, decode )
import qualified Database.PostgreSQL.Simple as Database

import Data.ByteString.UTF8 (fromString)
import Data.ByteString.Char8 (putStr)

newtype AnalyzeResult = AnalyzeResult {
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


formJSON :: Text -> String
formJSON t = utf8Encode $ "{\"string\": \"" ++ unpack t ++ "\"}"

request :: Text -> URI -> Request_String
request t uri = setRequestBody (Request{ rqURI = uri,
                         rqMethod = GET,
                         rqHeaders = [],
                         rqBody = "" }) ("application/json", formJSON t)

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

-- rpInstructionToString subject object gender posttext (RPInstruction verb emoji) = rpReply emoji subject (applyPronouns gender verb) object ++ posttext

-- userIdToMention number name = "[" ++ name ++ "](tg://user?id=" ++ number ++ ")"

userToMention :: User -> String
userToMention u =  "[" ++ unpack (username u) ++ "](tg://user?id=" ++ show (telegram_id u) ++ ")"

rpInstructionToString :: RPInstruction -> User -> User -> Pronoun  -> Text
rpInstructionToString (RPInstruction verb emoji) object subject gender = pack $ unwords [emoji, "|", subject_name, gendered_verb, object_name]
  where subject_name = userToMention subject
        object_name = userToMention object
        gendered_verb = applyPronouns gender verb

userGender :: User -> Pronoun
userGender u = if pronoun u /= Unset then pronoun u else used_more
  where used_more = case (used_mask u, used_fem u) of (mask, fem) | mask > fem -> HeHim
                                                                  | fem > mask -> SheHer
                                                                  | otherwise -> Gendergap
printUTF :: Text -> IO ()
printUTF = Data.ByteString.Char8.putStr . Data.ByteString.UTF8.fromString . unpack

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  ReplyText user text -> model <# do
    conn <- liftIO dbGetConn
    user <- liftIO $ getUser conn user
    reply $ constructReply . unpack $ text

  ReplyRp object subject instruction posttext -> model <# do
    liftIO $ putStrLn "in reply rp"
    conn <- liftIO dbGetConn
    object  <- liftIO $ getUser conn object
    subject <- liftIO $ getUser conn subject
    let text = rpInstructionToString instruction object subject (userGender subject)

    liftIO $ printUTF text

    reply $ constructReply . unpack $ text `append` posttext


  AnalyzeText user text -> model <# liftIO (do
    genderUsage <- analyzeGender text
    conn <- dbGetConn
    user <- getUser conn user
    dbUpdateUser conn (updateUserPronounsUsed user genderUsage)
    return ())

  SetGender user p -> model <# do
    conn <- liftIO $ dbGetConn
    user <- liftIO $  getUser conn user
    _ <- liftIO $ dbUpdateUser conn (user {pronoun=p})
    reply $ constructReply "Местоимения успешно обновлены"
  NoAction -> model <# return ()

-- Provides consistency with the database, creating a new user if nessesary 
getUser :: Database.Connection -> UserFromMessage -> IO DataTypes.User
getUser conn
        UserFromMessage {from_message_telegram_id = userId,
                         from_message_username    = userName} = do
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


dbGetConn :: IO Database.Connection
dbGetConn = connectPostgreSQL "postgres://postgres:postgres@127.0.0.1:5432"

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
