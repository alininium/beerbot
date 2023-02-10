{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Actions (
    messageToCommand,
    commandToAction,
) where

import           DataTypes
import           Parser
import Debug.Trace
import           Data.Text                        (unpack, toLower)
import           Data.Maybe
import           Data.List (intercalate, isPrefixOf)

import           Telegram.Bot.API

import Control.Monad.Trans (liftIO)
import Telegram.Bot.API (User(userFirstName))

userIdToMention :: String -> String -> String
userIdToMention number name = "[" ++ name ++ "](tg://user?id=" ++ number ++ ")"

userNameToMention :: String -> String
userNameToMention name = "[" ++ name ++ "](https://t.me/" ++  name ++ ")"

userIdToString :: UserId -> String
userIdToString (UserId int) = show int

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace a b s = replace' a b s []
    where replace' a b (s:st) h = if isPrefixOf a (s:st) then h ++ b ++ (removePrefix a (s:st))
                                                         else replace' a b st (h++[s])
          replace' _ _ [] h = h
          removePrefix (a:at) (s:st) = removePrefix at st
          removePrefix [] s = s

applyPronouns :: Pronoun -> String -> String
applyPronouns pronoun = replace "@" ending
    where ending =  case pronoun of HeHim     -> ""
                                    SheHer    -> "а"
                                    TheyThem  -> "и"
                                    ItIts     -> "о"
                                    Gendergap -> "(а)"
                                    Unset     -> "(а)"

userLinkFromMessage :: Message -> Maybe String
userLinkFromMessage message = do
  user <- messageFrom message
  let userName = userFirstName user
  let userIdNumber = userIdToString . userId $ user
  Just . userIdToMention userIdNumber . unpack $ userName

rpReply :: Emoji -> Subject -> Verb -> Object -> String
rpReply emoji subject verb object = unwords [emoji, "|", subject, verb, object]

messageToCommand :: Message -> Maybe Command
messageToCommand message = messageText message >>= readInput . unpack . toLower

rpInstructionToString :: Subject -> Object -> Pronoun -> String -> RPInstruction -> String
rpInstructionToString subject object gender posttext (RPInstruction verb emoji) = (rpReply emoji subject (applyPronouns gender verb) object) ++ posttext

extractUserGender :: [(String, Pronoun)] -> String -> Pronoun
extractUserGender ((s, p):mt) user = if user == s then p else extractUserGender mt user
extractUserGender [] user = Gendergap

commandToAction :: Message -> Command -> Maybe Action
commandToAction message (RP instruction posttext) =  do
    replyMessage <- messageReplyToMessage message

    object <- userLinkFromMessage replyMessage
    subject <- userLinkFromMessage message

    from <- messageFrom message
    username <- unpack <$> userUsername from
    let gender = extractUserGender genderMapping username

    Just . ReplyText $ trace ((show subject) ++ " " ++ (show gender)) $ rpInstructionToString subject object gender posttext instruction

commandToAction message (RPTargeted instruction targetName) = do
    let object = userNameToMention targetName
    subject <- userLinkFromMessage message
    let gender = extractUserGender genderMapping subject

    --pipe <- liftIO $ DB.connect (DB.host "127.0.0.1")
    --e <- trace "connecting" $ liftIO $ DB.access pipe DB.master "pivobot" run
    --DB.close pipe
    from <- messageFrom message
    username <- unpack <$> userUsername from


    let gender = extractUserGender genderMapping username
    let text = rpInstructionToString subject object gender "" instruction


    Just . ReplyText $ text

{-
-- {- commandToAction message (RPMultiple instructions) = do
    replyMessage <- messageReplyToMessage message

    object <- userLinkFromMessage replyMessage
    subject <- userLinkFromMessage message

    from <- messageFrom message
    username <- unpack <$> userUsername from

    let gender = extractUserGender genderMapping username
    let text = intercalate "\n" $ fmap (rpInstructionToString subject object gender) instructions
    Just . ReplyText $ text -}
-}


commandToAction message (RegularMessageWithI txt) = do
    from <- messageFrom message
    user <- userFirstName <$> messageFrom message
    let userId = userIdToInt . Telegram.Bot.API.userId $ from
    if txt == "" then Just NoAction else return $ AnalyzeText txt user userId

commandToAction _ (SimpleResponse text) = Just . ReplyText $ text

userIdToInt :: Telegram.Bot.API.UserId -> Integer
userIdToInt (UserId id) = id 

