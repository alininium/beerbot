{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Analyzer (
    updateUser,
) where

import Network.HTTP
    ( getResponseBody,
      simpleHTTP,
      setRequestBody,
      Request(Request, rqURI, rqMethod, rqHeaders, rqBody),
      RequestMethod(GET),
      Request_String )
import Network.URI ( URI, parseURI )
import Data.ByteString.Lazy.UTF8 as BLU ( fromString )
import Data.Aeson ( FromJSON, decode )
import Data.Text  (unpack, Text)
import GHC.Generics ( Generic )
import Data.Maybe ( fromJust, fromMaybe)
import Codec.Binary.UTF8.String (utf8Encode)

import DataTypes

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

formJSON :: Text -> String
formJSON t = utf8Encode $ "{\"string\": \"" ++ unpack t ++ "\"}"

request :: Text -> URI -> Request_String
request t uri = setRequestBody (Request{ rqURI = uri,
                         rqMethod = GET,
                         rqHeaders = [],
                         rqBody = "" }) ("application/json", formJSON t)

analyzeGender :: Text -> IO PronounsUsed
analyzeGender text = do
    ersp <- Network.HTTP.simpleHTTP (request text . fromJust $ parseURI "http://localhost:8000/analyze")
    a <- BLU.fromString <$> getResponseBody ersp
    let resp = response <$> (Data.Aeson.decode a :: Maybe AnalyzeResult)
    return $ fromMaybe PronounsUsed {masc = 0, fem = 0, plur = 0} resp

updateUserPronounsUsed :: DataTypes.User -> PronounsUsed -> DataTypes.User
updateUserPronounsUsed u p = TelegramUser {
                          telegram_id = telegram_id u,
                          username    = username u,
                          pronoun     = pronoun u,
                          used_mask   = used_mask u + if masc p > fem p then 1 else 0,
                          used_fem    = used_fem  u + if masc p < fem p then 1 else 0,
                          used_plur   = used_plur u + if (masc p == 0 && fem p == 0) && (plur p > 0) then 1 else 0
}

updateUser :: Text -> User -> IO User
updateUser t user = do
    used_pronouns <- analyzeGender t
    return $ updateUserPronounsUsed user used_pronouns
