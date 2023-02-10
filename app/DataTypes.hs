
{-# LANGUAGE OverloadedStrings #-}
module DataTypes (
    Model,
    Emoji,
    Subject,
    Verb,
    Object,
    RPInstruction (..),
    Command (..),
    Action (..),
    Pronoun (..),
    User (..),
    UserFromMessage (..),
    userFromTuple,
    userToTuple,
    rpMapping,
    simpleResponseMapping,
    setGenderMapping
) where

import Data.List (intercalate)
import Data.Text (Text)

type Model = String

type Emoji = String 

type Subject = String 
type Verb = String 
type Object = String 

data RPInstruction = RPInstruction Verb Emoji deriving Show

data Command = RP RPInstruction String
             | RPTargeted RPInstruction Object 
             | SimpleResponse String  
             -- | RPMultiple [RPInstruction] 
             | RegularMessageWithI Text 
             | GenderChange Pronoun deriving Show

data Action
  = NoAction
  | AnalyzeText UserFromMessage Text
  | ReplyText UserFromMessage Text
  | ReplyRp UserFromMessage UserFromMessage RPInstruction Text
  | SetGender UserFromMessage Pronoun

data Pronoun = HeHim | SheHer | TheyThem | ItIts | Gendergap | Unset deriving (Show, Eq)



data User = TelegramUser {
  telegram_id :: Integer,
  username :: Text,
  pronoun :: Pronoun,
  used_mask :: Integer,
  used_fem :: Integer,
  used_plur :: Integer
}  deriving Show

data UserFromMessage = UserFromMessage {
  from_message_telegram_id :: Integer,
  from_message_username:: Text
} deriving Show

pronounFromString :: Text -> Pronoun
pronounFromString s = case s of "he"        -> HeHim
                                "she"       -> SheHer
                                "they"      -> TheyThem
                                "it"        -> ItIts
                                "gendergap" -> Gendergap
                                _           -> Unset

pronounToString :: Pronoun -> Text
pronounToString p = case p of HeHim     -> "he"
                              SheHer    -> "she"
                              TheyThem  -> "they"
                              ItIts     -> "it"
                              Gendergap -> "gendergap"
                              Unset     -> "unk"

userFromTuple :: (Integer, Text, Text, Integer, Integer, Integer) -> User
userFromTuple (id, username, pronoun, used_mask, used_fem, used_plur) = TelegramUser { 
    telegram_id = id 
  , username = username
  , pronoun = pronounFromString pronoun 
  , used_mask = used_mask
  , used_fem = used_fem
  , used_plur = used_plur
}

userToTuple :: User -> (Text, Text, Integer, Integer, Integer, Integer)
userToTuple TelegramUser{telegram_id = telegram_id,
  username = username,
  pronoun = pronoun,
  used_mask = used_mask,
  used_fem = used_fem,
  used_plur = used_plur} 
  = (username, pronounToString pronoun, used_mask, used_fem, used_plur, telegram_id)



rpMapping :: [(String, RPInstruction)]
rpMapping = 
  [ ("обнять",                    RPInstruction "обнял@"                             "🤗")
  , ("дать леща",                 RPInstruction "дал@ леща"                          "🐟")
  , ("сломать колени",            RPInstruction "сломал@ колени"                     "🦵")
  , ("записать на ноготочки",     RPInstruction "записал@ на ноготочки"              "💅")
  , ("убить",                     RPInstruction "убил@"                              "☠️")
  , ("захуярить",                 RPInstruction "захуярил@"                          "💀")
  , ("расстрелять",               RPInstruction "расстрелял@"                        "🔫")
  , ("поцеловать",                RPInstruction "поцеловал@"                         "😘")
  , ("укусить",                   RPInstruction "укусил@"                            "😬")
  , ("куснуть",                   RPInstruction "укусил@"                            "😬")
  , ("кусь",                      RPInstruction "кусьнул@"                           "😬")
  , ("кусьнуть",                  RPInstruction "кусьнул@"                           "😬")
  , ("пнуть",                     RPInstruction "пнул@"                              "👞")
  , ("прижать",                   RPInstruction "прижал@"                            "🤲")
  , ("погладить",                 RPInstruction "погладил@"                          "🤲")
  , ("потрогать",                 RPInstruction "потрогал@"                          "🙌")
  , ("лизнуть",                   RPInstruction "лизнул@"                            "👅")
  , ("понюхать",                  RPInstruction "понюхал@"                           "👃")
  , ("ударить",                   RPInstruction "ударил@"                            "🤜😵")
  , ("шлепнуть",                  RPInstruction "шлепнул@"                           "👏")
  , ("взять на ручки",            RPInstruction "взял@ на ручки"                     "🤲")
  , ("шлёпнуть",                  RPInstruction "шлёпнул@"                           "👏")
  , ("майяшить",                  RPInstruction "отмайяшила"                         "💞")
  , ("гильотинировать",           RPInstruction "отрубил@ голову на гильотине"       "🔪🇲🇫")
  , ("предложить пива",           RPInstruction "предложил@ пива"                    "🍻")
  , ("пустить мицелий",           RPInstruction "пустил@ мицелий в"                  "🍄")
  , ("дефенестрировать",          RPInstruction "отправил@ в свободное падение"      "🏠")
  --, ("повесить",                  RPInstruction "повесил@"      "🪢")
  ]


setGenderMapping :: [(String, Pronoun)]
setGenderMapping = 
  [ (word ++ " " ++ secondWord , gender) | word <- ["пронауны", "пронаун", "местоимения", "гендер", "пронаунс", "род", "пол"],
                                          (secondWord, gender) <- [ ("-", Unset)
                                                                  , ("гендергеп", Gendergap)
                                                                  , ("гендергэп", Gendergap)
                                                                  , ("они", TheyThem)
                                                                  , ("она", SheHer)
                                                                  , ("оно", ItIts)
                                                                  , ("он", HeHim)
                                                                  , ("жен", SheHer)
                                                                  , ("муж", HeHim)
                                                                  , ("м", HeHim)
                                                                  , ("ж", SheHer)
                                          ]

  ]


getStringFromMapping :: [(String, a)] -> String
getStringFromMapping a = intercalate "\n" $ map fst a

helpText :: String
helpText = "Доступные команды:\n\n" ++ getStringFromMapping simpleResponseMapping

helpTextRp :: String
helpTextRp = "рп команды:\n\n" ++ getStringFromMapping rpMapping

simpleResponseMapping :: [(String, String)]
simpleResponseMapping = 
  [ ("пинг", "Понг")
  , ("спокойной ночи", "Cладких снов 🥺")
  , ("слава партии", "Слава Партии!")
  , ("кто здесь власть", "ПАРТИЯ!")
  , ("слава партии!", "Слава Партии!")
  , ("хочу умереть", "не надо 🥺")
  , ("я хочу умереть", "не надо 🥺")
  , ("хочу сдохнуть", "не надо 🥺")
  , ("я хочу сдохнуть", "не надо 🥺")
  , ("исходный код", "https://github.com/alininium/beerbot")
  , ("помощь рп", helpTextRp)
  , ("команды рп", helpTextRp)
  , ("помощь", helpText)
  , ("команды", helpText)
  , ("местоимения", "введите\nместоимения он/она/оно/они/гендергеп\nчтобы поменять свои местоимения")
  ]
