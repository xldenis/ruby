module Ruby.Parser where
  import Text.Megaparsec
  import Text.Megaparsec.Text
  import Text.Megaparsec.Expr (makeExprParser)

  import Control.Monad (void)

  import Ruby.AST
  import Ruby.Parser.Lexer
  import Ruby.Parser.Literal (parseLiteral)
  import Ruby.Parser.Operator (opTable)

  constantName :: Parser ConstantName
  constantName = f <$> lexeme (capitalized `sepBy` symbol "::")
    where f [n] = Name n
          f (n:ns) = Namespace n (f ns)

  self :: Parser Expression
  self = symbol "self" *> return Self

  raise :: Parser Expression
  raise = Raise <$> (symbol "raise" *> parseExpression)

  retry :: Parser Expression
  retry = symbol "retry" *> return Retry

  redo :: Parser Expression
  redo = symbol "redo"  *> return Redo

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
    args <- parens arguments' <|> arguments'
    sep
    body <- parseExpressions
    return $ Method name args body

  arguments' :: Parser [Arg]
  arguments' = list argument

  argument :: Parser Arg
  argument = keywordArg <|> basicArg

  basicArg :: Parser Arg
  basicArg = do
    name   <- identifier
    defVal <- optional $ symbol "=" *> parseExpression

    return $ Basic name defVal

  keywordArg :: Parser Arg
  keywordArg = do
    name <- try revSym
    defVal <- optional parseExpression

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

  parseIf :: Parser Expression
  parseIf = endBlock (symbol "if") $ do
    left <- Guard <$> (parseExpression <* sep) <*> parseExpressions
    branches <- many $ do
      symbol "elsif"
      cond <- parseExpression
      sep
      right <- parseExpressions
      return $ Guard cond right

    cap <- option Nil $ Else <$> (symbol "else" *> sep *> parseExpressions)

    return . If $ left (foldr (\a b -> a b) cap branches)

  parseUnless :: Parser Expression
  parseUnless = endBlock (symbol "unless") $ do
    left <- Guard <$> (parseExpression <* sep) <*> parseExpressions
    cap <- option Nil $ Else <$> (symbol "else" *> sep *> parseExpressions)

    return . Unless $ left cap

  block :: Parser Expression
  block = endBlock (symbol "do") $ do
    lhs <- concat <$> optional blockArgs
    sep
    body <- parseExpressions

    return $ Block lhs body

  inlineBlock :: Parser Expression
  inlineBlock = braces $ do
    lhs <- concat <$> optional blockArgs
    body <- parseExpressions

    return $ InlineBlock lhs body

  blockArgs :: Parser [Name]
  blockArgs = pipes $ list identifier

  require :: Parser Expression
  require = do
    symbol "require"
    path <- parseExpression
    return $ Require path

  selector :: Expression -> Parser Expression
  selector object = try $ do
    symbol "."
    method <- methodIdentifier
    return $ Dot object method

  invoke :: Expression -> Parser Expression
  invoke object = try $ do
    args <- list parseExpression
    return $ Invoke object args

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
    ensure <- optional $ (symbol "ensure" >> sep) *>
      parseExpressions
    return $ Begin body rescues ensure elseBlock

  parseRescue :: Parser Rescue
  parseRescue = do
    symbol "rescue"
    classes <- list parseExpression <* sc
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
    return $ Module name body

  parseClass :: Parser Expression
  parseClass = endBlock (symbol "class") $ do
    name <- constantName
    super <- optional $ symbol "<" *> constantName
    scn
    body <- parseExpressions
    return $ Class name super body

  parseProgram :: Parser Expression
  parseProgram = scn *> parseExpressions

  parseExpressions :: Parser Expression
  parseExpressions =
    Seq <$> parseExpression `endBy` sep

  parseExpression :: Parser Expression
  parseExpression = do
    op <- makeExprParser parseTerm opTable
    parseExpression' op

  parseExpression' :: Expression -> Parser Expression
  parseExpression' op = do
    ex <- optional (selector op)
    case ex of
      Nothing -> return op
      Just x -> parseExpression' x

  parseTerm :: Parser Expression
  parseTerm = defined
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
           <|> retry
           <|> parseIf
           <|> parseUnless
           <|> assignment
           <|> parseLiteral
