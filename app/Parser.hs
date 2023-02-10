{-# LANGUAGE OverloadedStrings #-}
module Parser (
    readInput,
    deb,
) where

import           DataTypes
import           Debug.Trace
import           Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Data.Text (Text, intercalate, pack, append, replace)

spaces :: Parser ()
spaces = skipMany1 space

parserFromMap :: [(String, a)] -> Parser a
parserFromMap = choice . map mappingToParser
  where mappingToParser (k, v) = try (string k) >> return v

rpInstruction :: Parser RPInstruction
rpInstruction = parserFromMap rpMapping

simpleResponseInstruction :: Parser String
simpleResponseInstruction = parserFromMap simpleResponseMapping

rpCommand :: Parser Command
rpCommand = try $ do
  rp <- try rpInstruction
  postfix <- try (many anyChar)
  return $ RP rp postfix


targetedRpCommand :: Parser Command
targetedRpCommand = try $ do
  rp <- rpInstruction
  spaces
  _ <- char '@'
  target <- many1 letter
  return $ RPTargeted rp target

simpleCommandPrompt :: Parser Command
simpleCommandPrompt = try $ SimpleResponse <$> simpleResponseInstruction


upToI :: Parser Text
upToI = pack . reverse <$> ("" <$ char 'я' <|> upToI' "")
  where {upToI' t = trailingSpaceI <|> nextChar
        where trailingSpaceI = t <$ try (string " я")
              nextChar = do
                c <- noneOf "."
                upToI' (c:t)
  }


sentenseWithI :: Parser Text
sentenseWithI = try $ do
  before <- try upToI
  _ <- oneOf " ." <|> ' ' <$ eof
  after <- pack <$> many (noneOf ".")
  _ <- Control.Monad.void (char '.') <|> eof
  return $ before `append` " я " `append` after `append` ". "

sentenseWithNoI :: Parser Text
sentenseWithNoI = try $ do
  _ <- many1 (noneOf ".я")
  _ <- Control.Monad.void (char '.') <|> eof
  return ""

regularMessageWithI :: Parser Command
regularMessageWithI = try $ RegularMessageWithI . Data.Text.replace "\n" " " . intercalate "." . filter (/="") <$> parser
  where parser = many1 (sentenseWithI <|> sentenseWithNoI <|> singleDot)
        singleDot = "" <$ pack . (:[]) <$> char '.'


-- jrpMultiple :: Parser Command
-- jrpMultiple = try $ RPMultiple <$> sepBy1 rpInstruction (try (char ' ') <|> try newline)

command :: Parser Command
command = (targetedRpCommand <|>  simpleCommandPrompt <|> rpCommand <|> regularMessageWithI) <* eof

deb :: String -> Maybe Text
deb i = case parse sentenseWithI "name" i of
  Left err -> Nothing
  Right c -> Just c

readInput :: String -> Maybe Command
readInput input = case parse command "name" input of
  Left err -> Nothing
  Right c -> Just c