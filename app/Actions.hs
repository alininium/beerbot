module Actions (
    messageToCommand,
    commandToAction,
) where

import           DataTypes
import           Parser
import           Data.Text                        (unpack, toLower)
import           Data.Maybe
import           Data.List                        (intersperse)

import           Telegram.Bot.API

userIdToMention :: String -> String -> String
userIdToMention number name = "[" ++ name ++ "](tg://user?=id=" ++ number ++ ")"

userNameToMention :: String -> String 
userNameToMention name = "[" ++ name ++ "](https://t.me/" ++  name ++ ")"

userIdToString :: UserId -> String
userIdToString (UserId int) = show int

userLinkFromMessage :: Message -> Maybe String
userLinkFromMessage message = do 
  user <- messageFrom message
  let userName = userFirstName user
  let userIdNumber = userIdToString . userId $ user
  Just . userIdToMention userIdNumber . unpack $ userName
  
rpReply :: Emoji -> Subject -> Verb -> Object -> String
rpReply emoji subject verb object = foldl1 (\x y -> x ++ ' ':y) [emoji, "|", subject, verb, object]


messageToCommand :: Message -> Maybe Command
messageToCommand message = messageText message >>= readInput . unpack . toLower

commandToAction :: Message -> Command -> Maybe Action
commandToAction message (RP (RPInstruction verb emoji)) = do
    replyMessage <- messageReplyToMessage message

    object <- userLinkFromMessage replyMessage
    subject <- userLinkFromMessage message
    Just . ReplyText $ rpReply emoji subject verb object

commandToAction message (RPTargeted (RPInstruction verb emoji) targetName) = do
    let object = userNameToMention targetName
    subject <- userLinkFromMessage message
    Just . ReplyText $ rpReply emoji subject verb object

commandToAction message (SimpleResponse text) = Just . ReplyText $ text

