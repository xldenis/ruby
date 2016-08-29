module Ruby.Parser where
  import Ruby.Parser.Lexer

  import Ruby.AST

  import Text.Megaparsec
  import Text.Megaparsec.Text

  import Control.Monad (void)

  self :: Parser Expression
  self = symbol "self" *> (return Self)

  parseModule :: Parser Expression
  parseModule = endBlock (symbol "module") $ do
    name <- constantName
    sep
    expr <- parseExpressions
    return $ Module name expr

  parseClass :: Parser Expression
  parseClass = do
    symbol "class"
    name <- constantName
    super <- optional $ symbol "<" *> constantName
    body <- parseExpressions
    return $ Class name super body

  parseExpressions :: Parser Expression
  parseExpressions = do
    Seq <$> parseExpression `sepBy` sep

  parseExpression :: Parser Expression
  parseExpression = parseModule <|> self

  constantName :: Parser ConstantName
  constantName = f <$> (lexeme $ capitalized `sepBy` (symbol "::"))
    where f (n:[]) = Name n
          f (n:ns) = Namespace n (f ns)
