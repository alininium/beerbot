{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import DataTypes
    ( Action(..),
      Model,
      RPInstruction(..),
      Pronoun(Unset, HeHim, SheHer, Gendergap),
      User(..) )
import Analyzer
import DB
import Actions ( commandToAction, messageToCommand, applyPronouns )

import           Data.Text                        (pack, unpack, Text, append, replace)
import Data.Maybe ( isJust )
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


import Data.ByteString.UTF8 (fromString)
import Data.ByteString.Char8 (putStr)


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

userToMention :: User -> String
userToMention u =  "[" ++ unpack uname ++ "](tg://user?id=" ++ show (telegram_id u) ++ ")"
  where uname = (replace "[" "" . replace "]" "" . username)  u

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
    reply $ constructReply . unpack $ text

  ReplyRp object subject instruction posttext -> model <# do
    liftIO $ putStrLn "in reply rp"
    object  <- liftIO $ iUpdateUser object
    subject <- liftIO $ iUpdateUser subject
    let text = rpInstructionToString instruction object subject (userGender subject)

    liftIO $ printUTF text

    reply $ constructReply . unpack $ text `append` posttext


  AnalyzeText user text -> model <# liftIO (DB.updateUserWithIO user (Analyzer.updateUser text))

  SetGender user p -> model <# do
    _ <- liftIO $ DB.updateUserWithIO user (pure . setPronoun p)
    reply $ constructReply "Местоимения успешно обновлены"
  NoAction -> model <# return ()

setPronoun :: Pronoun -> User -> User
setPronoun p u = u {pronoun=p}



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
