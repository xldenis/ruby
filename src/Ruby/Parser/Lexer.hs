module Ruby.Parser.Lexer where
  import qualified Text.Megaparsec.Lexer as L
  import Text.Megaparsec.Text
  import Text.Megaparsec

  import Control.Monad (void)
  import Control.Applicative ((<*), empty)

  reserved :: [String]
  reserved = ["if", "then", "else", "end", "def"]

  identifier :: Parser String
  identifier = p >>= res
    where p = label "identifier" . lexeme $ ((:) <$> letterChar <*> many alphaNumChar)
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
  sep = scn
  -- sep = skipSome (symbol ";" *> return () <|> scn)

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
