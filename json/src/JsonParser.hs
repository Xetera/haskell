{-# LANGUAGE FlexibleContexts #-}

module JsonParser (run) where

import Data.Text (Text)
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Char (letter)

data Json
  = JNum Int
  | JArray [[Json]]
  | JObject [(String, Json)]
  | JKey String
  | JBool Bool
  | JNull
  deriving (Show)

value =
      JArray       <$> array
  <|> JObject      <$> object
  <|> JKey         <$> quotedString
  <|> JBool  True  <$  string "true"
  <|> JBool  False <$  string "false"
  <|> JNull        <$  string "null"
  <|> JNum . read  <$> many1 alphaNum

quote = char '"'

validStringChar  = letter <|> alphaNum <|> space
withQuotes p     = between quote quote p
ignoringSpaces p = spaces *> p <* spaces
quotedString     = withQuotes $ many validStringChar

kv = do
  spaces
  key <- quotedString
  spaces
  char ':'
  spaces
  val <- value
  spaces
  rest <- optionMaybe $ char ',' *> kv
  spaces
  return $ (key, val) : case rest of
                          Just e -> e
                          _ -> []

array = between (char '[') (char ']') $
  many $ ignoringSpaces value `sepBy` ignoringSpaces (char ',')

object = between (char '{') (char '}') $ ignoringSpaces kv

input = "{\"test 1321 \": \"1234\", \"y\": { \"we\": \"did it boys\" }  }"
--input = "[ 1 , [ false ,3 ], 3,[4,[[]]],  5   ]"

parseJson = object

run :: IO ()
run = print $ parse parseJson "" input
