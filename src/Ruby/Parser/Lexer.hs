module Ruby.Parser.Lexer (
    module Ruby.Parser.Lexer
  , char
  , oneOf
  , Text
  , pack
  , unpack
) where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Data.Maybe                 (maybeToList)

import           Control.Applicative        (empty, liftA2, (<*))
import           Control.Monad              (void)

import           Data.Text                  (Text, cons, pack, singleton,
                                             unpack)
import           Data.Void

import           Data.Maybe
import           Data.Semigroup

type Parser = Parsec Void Text

reserved :: [Text]
reserved = ["__ENCODING__", "__LINE__", "__FILE__", "BEGIN", "END", "alias", "and", "begin", "break", "case",
            "class", "def", "defined?", "do", "else", "elsif", "end", "ensure", "false", "for", "if", "in",
            "module", "next", "nil", "not", "or", "redo", "rescue", "retry", "return", "self", "super", "then",
            "true", "undef", "unless", "until", "when", "while", "yield"]

identifier :: Parser Text
identifier = p >>= res
  where p = label "identifier" . lexeme $ pack <$> ((:) <$> startLetter <*> (many midLetter))
        res i = if i `elem` reserved then
            fail $ "The reserved word `" <> unpack i <> "` cannot be used as an identifier."
          else
            return i

methodIdentifier :: Parser Text
methodIdentifier = label "method identifier" . lexeme $ (<>) . pack <$> ((:) <$> letterChar <*> many midLetter) <*> endLetter

symbolIdentifier :: Parser Text
symbolIdentifier = label "symbol" . lexeme $ (<>) . pack <$> ((:) <$> startLetter <*> many midLetter) <*> endLetter

startLetter :: Parser Char
startLetter = oneOf chars
  where chars = "@$_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" :: String

midLetter :: Parser Char
midLetter   = oneOf chars
  where chars =  "_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" :: String

endLetter :: Parser Text
endLetter = maybe mempty singleton <$> optional (oneOf ("!_=?" :: [Char]))

revSym :: Parser Text
revSym = label "keyword" . lexeme $ pack <$> ((:) <$> letterChar <*> many midLetter <* char ':')

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

capitalized :: Parser Text
capitalized = pack <$> ((:) <$> upperChar <*> many alphaNumChar)

symbol :: Tokens Text -> Parser Text
symbol = L.symbol sc

endBlock :: Parser a -> Parser b -> Parser b
endBlock open body = do
  open
  body <- body
  symbol "end"
  return body

scn :: Parser ()
scn = L.space (void spaceChar <|> sepChar) lineComment empty

sc :: Parser ()
sc = L.space (void $ oneOf (" \t" :: String)) lineComment empty

sep :: Parser ()
sep =  sepChar *> scn

sepChar :: Parser ()
sepChar = void $ oneOf ("\n;" :: String)

lineComment :: Parser ()
lineComment = char '#' *> skipMany (noneOf ("\n" :: [Char]))

list :: Parser a -> Parser [a]
list a = a `sepBy` symbol ","

list1 :: Parser a -> Parser [a]
list1 a = a `sepBy1` (symbol "," <* scn)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

squotes :: Parser a -> Parser a
squotes = between (char '\'') (char '\'')

dquotes :: Parser a -> Parser a
dquotes = between (char '"') (char '"')

pipes :: Parser a -> Parser a
pipes = between (symbol "|") (symbol "|")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")
