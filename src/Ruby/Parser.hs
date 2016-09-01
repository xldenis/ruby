module Ruby.Parser where
  import Text.Megaparsec
  import Text.Megaparsec.Text

  import Control.Monad (void)

  import Ruby.AST
  import Ruby.Parser.Lexer
  import Ruby.Parser.Literal

  self :: Parser Expression
  self = symbol "self" *> return Self

  raise :: Parser Expression
  raise = Raise <$> (symbol "raise" *> parseExpression)

  parseReturn :: Parser Expression
  parseReturn = Return <$> (symbol "return" *> list parseExpression)

  yield :: Parser Expression
  yield = Yield <$> (symbol "yield" *> list parseExpression)

  parseBreak :: Parser Expression
  parseBreak = Break <$> (symbol "break" *> list parseExpression)

  next :: Parser Expression
  next = Next <$> (symbol "next" *> list parseExpression)

  alias :: Parser Expression
  alias = do
    symbol "alias"
    new <- identifier
    old <- identifier
    return $ Alias new old

  defineMethod :: Parser Expression
  defineMethod = endBlock (symbol "def") $ do
    name <- methodIdentifier
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

  require :: Parser Expression
  require = do
    symbol "require"
    path <- parseExpression
    return $ Require path

  assignment :: Parser Expression
  assignment = try $ do
    lhs <- list variables
    symbol "="
    rhs <- list parseExpression

    return $ Assign lhs rhs
    where variables = identifier

  begin :: Parser Expression
  begin = endBlock (symbol "begin") $ do
    sep
    body <- parseExpressions
    rescues <- many parseRescue
    sep
    elseBlock <- optional $ (symbol "else" >> sep) *> parseExpressions
    sep
    ensure <- optional $ (symbol "ensure" >> sep) *> do
      parseExpressions
    return $ Begin body rescues ensure elseBlock

  parseRescue :: Parser Rescue
  parseRescue = do
    symbol "rescue"
    classes <- (list parseExpression) <* sc
    variable <- optional $ symbol "=>" *> identifier
    sep
    body <- parseExpressions

    return $ case variable of
      Nothing      -> ExceptionClasses classes body
      Just varName -> ExceptionBinding classes varName body

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
  parseExpression = defined
                 <|> defineMethod
                 <|> parseModule
                 <|> parseClass
                 <|> begin
                 <|> self
                 <|> raise
                 <|> parseReturn
                 <|> yield
                 <|> next
                 <|> parseBreak
                 <|> undefine
                 <|> inlineBlock
                 <|> block
                 <|> alias
                 <|> require
                 <|> assignment
                 <|> parseLiteral

  constantName :: Parser ConstantName
  constantName = f <$> (lexeme $ capitalized `sepBy` (symbol "::"))
    where f (n:[]) = Name n
          f (n:ns) = Namespace n (f ns)
