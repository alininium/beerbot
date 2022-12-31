
module DataTypes (
    Model,
    Emoji,
    Subject,
    Verb,
    Object,
    RPInstruction (..),
    Command (..),
    Action (..),
    rpMapping,
    simpleResponseMapping
) where

import Data.List (intercalate)

type Model = ()

type Emoji = String 

type Subject = String 
type Verb = String 
type Object = String 

data RPInstruction = RPInstruction Verb Emoji deriving Show

data Command = RP RPInstruction
             | RPTargeted RPInstruction Object 
             | SimpleResponse String  
             | RPMultiple [RPInstruction] deriving Show

data Action
  = NoAction
  | ReplyText String


rpMapping :: [(String, RPInstruction)]
rpMapping = 
  [ ("обнять",                    RPInstruction "обнял(а)"                             "🤗")
  , ("дать леща",                 RPInstruction "дал(а) леща"                          "🐟")
  , ("сломать колени",            RPInstruction "сломал(а) колени"                     "🦵")
  , ("записать на ноготочки",     RPInstruction "записал(a) на ноготочки"              "💅")
  , ("убить",                     RPInstruction "убил(а)"                              "☠️")
  , ("расстрелять",               RPInstruction "расстрелял(а)"                        "🔫")
  , ("поцеловать",                RPInstruction "поцеловал(а)"                         "😘")
  , ("кусь",                      RPInstruction "кусьнул(а)"                           "😬")
  , ("кусьнуть",                  RPInstruction "кусьнул(а)"                           "😬")
  , ("укусить",                   RPInstruction "укусил(а)"                            "😬")
  , ("пнуть",                     RPInstruction "пнул(а)"                              "👞")
  , ("прижать",                   RPInstruction "прижал(а)"                            "🤲")
  , ("погладить",                 RPInstruction "погладил(а)"                          "🤲")
  , ("потрогать",                 RPInstruction "потрогал(а)"                          "🙌")
  , ("лизнуть",                   RPInstruction "лизнул(а)"                            "👅")
  , ("понюхать",                  RPInstruction "понюхал(а)"                           "👃")
  , ("ударить",                   RPInstruction "ударил(а)"                            "🤜😵")
  , ("шлепнуть",                  RPInstruction "шлепнул(а)"                           "👏")
  , ("шлёпнуть",                  RPInstruction "шлёпнул(а)"                           "👏")
  , ("майяшить",                  RPInstruction "отмайяшила"                           "💞")
  , ("гильотинировать",           RPInstruction "отрубил(а) голову на гильотине"       "🔪🇲🇫")
  , ("предложить пива",           RPInstruction "предложил(а) пива"                    "🍻")
  , ("пустить мицелий",           RPInstruction "пустил(а) мицелий в"                  "🍄")
  , ("дефенестрировать",          RPInstruction "отправил(а) в свободное падение"      "🏠")
  ]


helpText :: String
helpText = "Доступные рп команды:\n\n" ++ ( intercalate "\n" $ map fst rpMapping)

simpleResponseMapping :: [(String, String)]
simpleResponseMapping = 
  [ ("хочу умереть", "не надо 🥺")
  , ("я хочу умереть", "не надо 🥺")
  , ("хочу сдохнуть", "не надо 🥺")
  , ("я хочу сдохнуть", "не надо 🥺")
  , ("пинг", "Понг")
  , ("спокойной ночи", "Cладких снов 🥺")
  , ("слава партии", "Слава Партии!")
  , ("cлава партии!", "Слава Партии!")
  , ("помощь", helpText)
  ]
