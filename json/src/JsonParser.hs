{-# LANGUAGE FlexibleContexts #-}

module JsonParser (run) where

import Data.Text (Text)
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Char (letter)

data JComposite
  = JArray [[JValue]]
  | JObject [(String, JValue)]
  deriving (Show)

data JAtom
  = JNum Int
  | JKey String
  | JBool Bool
  | JNull
  deriving (Show)

data JValue = Atom JAtom | Composite JComposite deriving (Show)

--value ::ParsecT [Char] a Identity JValue
value =
      JArray    <$> array
  <|> JObject    <$> object
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
  rest <- try $ char ',' *> kv
  spaces
  return $ (key, val) : rest

array = between (char '[') (char ']') $
  many $ ignoringSpaces value `sepBy` ignoringSpaces (char ',')

object = between (char '{') (char '}') $ ignoringSpaces kv

input = "{\"test 1321 \": 123, \"y\": \"eet\" }"
--input = "[ 1 , [ false ,3 ], 3,[4,[[]]],  5   ]"

parseJson = Composite <$> array <|> object

run :: IO ()
run = print $ parse parseJson "" input
