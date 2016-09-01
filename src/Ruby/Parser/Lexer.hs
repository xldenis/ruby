module Ruby.Parser.Lexer where
  import qualified Text.Megaparsec.Lexer as L
  import Text.Megaparsec.Text
  import Text.Megaparsec

  import Data.Maybe (maybeToList)

  import Control.Monad (void)
  import Control.Applicative ((<*), empty)

  reserved :: [String]
  reserved = ["__ENCODING__", "__LINE__", "__FILE__", "BEGIN", "END", "alias", "and", "begin", "break", "case",
              "class", "def", "defined?", "do", "else", "elsif", "end", "ensure", "false", "for", "if", "in",
              "module", "next", "nil", "not", "or", "redo", "rescue", "retry", "return", "self", "super", "then",
              "true", "undef", "unless", "until", "when", "while", "yield"]

  identifier :: Parser String
  identifier = p >>= res
    where p = label "identifier" . lexeme $ ((:) <$> letterChar <*> many alphaNumChar)
          res i = if i `elem` reserved then
              fail $ "The reserved word `" ++ i ++ "` cannot be used as an identifier."
            else
              return i

  methodIdentifier :: Parser String
  methodIdentifier = label "method identifier" . lexeme $ (++) <$> ((:) <$> startLetter <*> many midLetter) <*> endLetter
    where startLetter = oneOf "@$_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
          midLetter   = oneOf "_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
          endLetter   = maybeToList <$> optional (oneOf "!_=?") :: Parser String

  revSym :: Parser String
  revSym = p >>= res
    where p = label "symbol" . lexeme $ ((:) <$> letterChar <*> many alphaNumChar <* char ':')
          res i = if i `elem` reserved then
              fail $ "The reserved word `" ++ i ++ "` cannot be used as an identifier."
            else
              return i

  lexeme :: Parser a -> Parser a
  lexeme = L.lexeme sc

  capitalized :: Parser String
  capitalized = (:) <$> upperChar <*> many alphaNumChar

  symbol :: String -> Parser String
  symbol = L.symbol sc

  endBlock :: Parser a -> Parser b -> Parser b
  endBlock open body = do
    open
    body <- body
    sep
    symbol "end"
    return body

  scn :: Parser ()
  scn = L.space (void spaceChar) lineComment empty

  sc :: Parser ()
  sc = L.space (void $ oneOf " \t") lineComment empty

  sep :: Parser ()
  sep = L.space (void spaceChar <|> void (symbol ";")) lineComment empty

  lineComment :: Parser ()
  lineComment = char '#' *> skipMany (noneOf "\n")

  list :: Parser a -> Parser [a]
  list a = a `sepBy` symbol ","

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
