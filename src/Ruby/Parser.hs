module Ruby.Parser where
  import Text.Megaparsec
  import Text.Megaparsec.Text
  import Text.Megaparsec.Expr (makeExprParser)

  import Control.Monad (void)

  import Data.Maybe (isJust)

  import Ruby.AST
  import Ruby.Parser.Lexer
  import Ruby.Parser.Literal (parseLiteral)
  import Ruby.Parser.Operator (opTable)

  constantName :: Parser ConstantName
  constantName = f <$> lexeme (capitalized `sepBy1` symbol "::")
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

  super :: Parser Expression
  super = symbol "super" *> return Super

  parseReturn :: Parser Expression
  parseReturn = Return <$> (symbol "return" *> list parseExpression)

  yield :: Parser Expression
  yield = Yield <$> (symbol "yield" *> list parseExpression)

  parseBreak :: Parser Expression
  parseBreak = Break <$> (symbol "break" *> list parseExpression)

  next :: Parser Expression
  next = Next <$> (symbol "next" *> list parseExpression)

  parseConstant :: Parser Expression
  parseConstant = Constant <$> constantName

  alias :: Parser Expression
  alias = do
    symbol "alias"
    new <- identifier
    old <- identifier
    return $ Alias new old

  parseMethod :: Parser Expression
  parseMethod = endBlock (symbol "def") $ do
    isSelf <- isJust <$> (optional $ symbol "self" *> symbol ".")
    name <- methodIdentifier
    args <- parens arguments' <|> arguments'
    sep
    body <- parseExpressions
    return $ Method name args body isSelf

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

    cap <- option End $ Else <$> (symbol "else" *> sep *> parseExpressions)

    return . If $ left (foldr (\a b -> a b) cap branches)

  parseUnless :: Parser Expression
  parseUnless = endBlock (symbol "unless") $ do
    left <- Guard <$> (parseExpression <* sep) <*> parseExpressions
    cap <- option End $ Else <$> (symbol "else" *> sep *> parseExpressions)

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
    args <- parens argList <|> argList
    return $ Invoke object args
    where argList = list1 (scn *> parseExpression)
  ifGuard :: Expression -> Parser Expression
  ifGuard object = do
    symbol "if"
    condition <- parseExpression
    return $ IfMod object condition

  unlessGuard :: Expression -> Parser Expression
  unlessGuard object = do
    symbol "unless"
    condition <- parseExpression
    return $ UnlessMod object condition

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
    super <- optional $ symbol "<" *> parseExpression
    scn
    body <- parseExpressions
    return $ Class name super body

  localVariable :: Parser Expression
  localVariable = try $ do
    (Variable Local) <$> identifier

  parseProgram :: Parser Expression
  parseProgram = scn *> parseExpressions

  parseExpressions :: Parser Expression
  parseExpressions =
    Seq <$> parseExpression `endBy` sep

  parseExpression :: Parser Expression
  parseExpression = do
    op <- makeExprParser parseTerm opTable
    return op

  parseTerm :: Parser Expression
  parseTerm = do
    term <- baseTerm
    leftTerm term

  leftTerm :: Expression -> Parser Expression
  leftTerm op = do
    ex <- optional (selector op <|> ifGuard op <|> unlessGuard op <|> invoke op)
    case ex of
      Nothing -> return op
      Just x -> leftTerm x

  baseTerm :: Parser Expression
  baseTerm = defined
           <|> parseMethod
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
           <|> parseConstant
           <|> localVariable
           <|> parens parseExpression
           <|> super
           <|> parseLiteral
