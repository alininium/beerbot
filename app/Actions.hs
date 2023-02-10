{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Actions (
    messageToCommand,
    commandToAction,
    applyPronouns,
) where

import           DataTypes
import           Parser
import Debug.Trace
import           Data.Text                        (pack, unpack, toLower)
import           Data.List (isPrefixOf)

import           Telegram.Bot.API


userIdToMention :: String -> String -> String
userIdToMention number name = "[" ++ name ++ "](tg://user?id=" ++ number ++ ")"

userNameToMention :: String -> String
userNameToMention name = "[" ++ name ++ "](https://t.me/" ++  name ++ ")"

userIdToString :: UserId -> String
userIdToString (UserId int) = show int

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace a b s = replace' a b s []
    where replace' a b (s:st) h = if a `isPrefixOf` (s:st) then h ++ b ++ removePrefix a (s:st)
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

getUserId :: Telegram.Bot.API.User -> Integer
getUserId user = case userId user of (UserId a) -> a

userFromMessage :: Message -> Maybe UserFromMessage
userFromMessage message = do
  user <- messageFrom message
  Just (UserFromMessage{from_message_telegram_id = getUserId user, from_message_username = userFirstName user})

rpReply :: Emoji -> Subject -> Verb -> Object -> String
rpReply emoji subject verb object = unwords [emoji, "|", subject, verb, object]

messageToCommand :: Message -> Maybe Command
messageToCommand message = messageText message >>= readInput . unpack . toLower


extractUserGender :: [(String, Pronoun)] -> String -> Pronoun
extractUserGender ((s, p):mt) user = if user == s then p else extractUserGender mt user
extractUserGender [] _ = Gendergap

commandToAction :: Message -> Command -> Maybe Action
commandToAction message (RP instruction posttext) =  do
    replyMessage <- messageReplyToMessage message

    object <- userFromMessage replyMessage
    subject <- userFromMessage message

    Just $ ReplyRp object subject instruction (pack posttext)

commandToAction message (RPTargeted instruction targetName) = do
    subject <- userFromMessage message
    Just $ ReplyText subject ""

commandToAction message (RegularMessageWithI txt) = do
    user <- userFromMessage message
    if txt == "" then Just NoAction else return $ AnalyzeText user txt

commandToAction message (GenderChange p) = do
    user <- userFromMessage message
    Just . SetGender user $ p

commandToAction message (SimpleResponse text) = do
    from <- messageFrom message
    username <- unpack <$> userUsername from
    let user_telegram_id = case userId from of (UserId a) -> a

    Just . ReplyText UserFromMessage{from_message_telegram_id = user_telegram_id, from_message_username = pack username} $ pack text

userIdToInt :: Telegram.Bot.API.UserId -> Integer
userIdToInt (UserId id) = id

