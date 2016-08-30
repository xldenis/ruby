module Ruby.Parser where
  import Ruby.Parser.Lexer

  import Ruby.AST

  import Text.Megaparsec
  import Text.Megaparsec.Text

  import Control.Monad (void)

  self :: Parser Expression
  self = symbol "self" *> return Self

  alias :: Parser Expression
  alias = do
    symbol "alias"
    new <- identifier
    old <- identifier
    return $ Alias new old

  defineMethod :: Parser Expression
  defineMethod = endBlock (symbol "def") $ do
    name <- identifier
    args <- parens arguments <|> arguments
    sep
    body <- parseExpressions
    return $ Method name args body

  arguments :: Parser [Arg]
  arguments = list $ argument

  argument :: Parser Arg
  argument = keywordArg <|> basicArg

  basicArg :: Parser Arg
  basicArg = do
    name   <- identifier
    defVal <- optional $ symbol "=" *> parseExpression

    return $ Basic name defVal

  keywordArg :: Parser Arg
  keywordArg = do
    name <- try $ revSym
    defVal <- optional $ parseExpression

    return $ Keyword name defVal

  undefine :: Parser Expression
  undefine = do
    symbol "undef"
    names <- list identifier
    return $ Undefine names

  defined :: Parser Expression
  defined = do
    symbol "defined?"
    expr <- parens parseExpression <|> parseExpression

    return $ Defined expr

  block :: Parser Expression
  block = endBlock (symbol "do") $ do
    lhs <- blockArgs
    sep
    body <- parseExpressions

    return $ Block lhs body

  inlineBlock :: Parser Expression
  inlineBlock = braces $ do
    lhs <- blockArgs
    body <- parseExpressions

    return $ InlineBlock lhs body

  blockArgs :: Parser [Name]
  blockArgs = pipes $ list identifier

  parseModule :: Parser Expression
  parseModule = endBlock (symbol "module") $ do
    name <- constantName <* sep
    body <- parseExpressions
    return $ Module name (body)

  parseClass :: Parser Expression
  parseClass = endBlock (symbol "class") $ do
    name <- constantName
    super <- optional $ symbol "<" *> constantName
    scn
    body <- parseExpressions
    return $ Class name super body

  parseExpressions :: Parser Expression
  parseExpressions = do
    Seq <$> parseExpression `endBy` sep

  parseExpression :: Parser Expression
  parseExpression = defined <|> defineMethod <|> parseModule <|> parseClass <|> self <|> undefine <|> inlineBlock <|> block <|> alias

  constantName :: Parser ConstantName
  constantName = f <$> (lexeme $ capitalized `sepBy` (symbol "::"))
    where f (n:[]) = Name n
          f (n:ns) = Namespace n (f ns)
