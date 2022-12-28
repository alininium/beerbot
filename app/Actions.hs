module Actions (
    messageToCommand,
    commandToAction,
) where

import           DataTypes
import           Parser
import Debug.Trace
import           Data.Text                        (unpack, toLower)
import           Data.Maybe
import           Data.List (intercalate)

import           Telegram.Bot.API

userIdToMention :: String -> String -> String
userIdToMention number name = "[" ++ name ++ "](tg://user?id=" ++ number ++ ")"

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
rpReply emoji subject verb object = unwords [emoji, "|", subject, verb, object]

messageToCommand :: Message -> Maybe Command
messageToCommand message = messageText message >>= readInput . unpack . toLower 

rpInstructionToString :: Subject -> Object -> RPInstruction -> String
rpInstructionToString subject object (RPInstruction verb emoji) = rpReply emoji subject verb object


commandToAction :: Message -> Command -> Maybe Action
commandToAction message (RP instruction) =  do
    replyMessage <- messageReplyToMessage message

    object <- userLinkFromMessage replyMessage
    subject <- userLinkFromMessage message
    Just . ReplyText $ rpInstructionToString subject object instruction

commandToAction message (RPTargeted (RPInstruction verb emoji) targetName) = do
    let object = userNameToMention targetName
    subject <- userLinkFromMessage message
    Just . ReplyText $ rpReply emoji subject verb object

commandToAction message (RPMultiple instructions) = do
    replyMessage <- messageReplyToMessage message

    object <- userLinkFromMessage replyMessage
    subject <- userLinkFromMessage message
    let text = intercalate "\n" $ fmap (rpInstructionToString subject object) instructions
    Just . ReplyText $ text



commandToAction _ (SimpleResponse text) = Just . ReplyText $ text

