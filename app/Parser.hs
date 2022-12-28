module Parser (
    readInput,
    rpMultiple
) where

import           DataTypes
import           Debug.Trace
import           Text.ParserCombinators.Parsec hiding (spaces)

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
  return $ RP rp

targetedRpCommand :: Parser Command
targetedRpCommand = try $ do
  rp <- rpInstruction
  spaces
  _ <- char '@'
  target <- many1 letter
  return $ RPTargeted rp target

simpleCommandPrompt :: Parser Command
simpleCommandPrompt = try $ SimpleResponse <$> simpleResponseInstruction

rpMultiple :: Parser Command
rpMultiple = try $ RPMultiple <$> sepBy1 rpInstruction (try (char ' ') <|> try newline)

command :: Parser Command
command = (targetedRpCommand <|> rpMultiple <|> rpCommand <|> simpleCommandPrompt) <* eof

readInput :: String -> Maybe Command
readInput input = case parse command "name" input of
  Left err -> Nothing
  Right c -> Just c