{-# LANGUAGE MultiWayIf #-}
module Main where

import           DataTypes
import           Actions

import           Data.Text                        (pack)
import           Data.Maybe

import           System.Environment.MrEnv (envAsString)
import           Configuration.Dotenv (loadFile, defaultConfig)
import           Telegram.Bot.API
import           Telegram.Bot.Simple
import           Telegram.Bot.API (User(userFirstName, userId))


echoBot :: BotApp Model Action
echoBot = BotApp
  { botInitialModel = ()
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

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  ReplyText text -> model <# do
    reply $ constructReply text
  NoAction -> model <# do
    return ()

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