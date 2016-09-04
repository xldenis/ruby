module Ruby.Parser where
  import Text.Megaparsec
  import Text.Megaparsec.Text
  import Text.Megaparsec.Expr (makeExprParser)

  import Control.Monad (void, when)

  import Data.Maybe (isJust)

  import Ruby.AST
  import Ruby.Parser.Lexer
  import Ruby.Parser.Literal (parseLiteral, variableLiteral)
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
    singleton <- (optional . try $ (choice [localVariable, keywordVariable, parens parseExpression])  <* symbol ".")
    name <- methodIdentifier
    args <- parens arguments' <|> (arguments' <* sep)
    scn
    body <- parseExpressions
    return $ Method name args body singleton

  arguments' :: Parser [Arg]
  arguments' = list argument

  argument :: Parser Arg
  argument = try $ keywordArg <|> basicArg

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
    scn
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
    when (isLiteral object) (fail "uncallable object")
    args <- parens (scn *> argList) <|> argList
    return $ Invoke object args
    where argList = list1 (parseExpression)

  isLiteral :: Expression -> Bool
  isLiteral (Literal _) = True
  isLiteral _ = False

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
    lhs <- list1 $ parseLeftHandTerm
    symbol "="
    rhs <- list1 parseExpression

    return $ Assign lhs rhs

  begin :: Parser Expression
  begin = endBlock (symbol "begin") $ do
    scn
    body <- parseExpressions
    scn
    rescues <- many (parseRescue <* scn)
    elseBlock <- optional $ (symbol "else" >> sep) *> parseExpressions
    ensure <- optional $ (symbol "ensure" >> scn) *>
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
    sep
    body <- parseExpressions
    return $ Class name super body

  localVariable :: Parser Expression
  localVariable = try $ do
    (Variable Local) <$> identifier

  userVariable :: Parser Expression
  userVariable = localVariable <|> parseConstant

  keywordVariable :: Parser Expression
  keywordVariable = variableLiteral <|> self

  parseProgram :: Parser Expression
  parseProgram = scn *> parseExpressions

  parseExpressions :: Parser Expression
  parseExpressions = Seq <$> parseExpression `sepEndBy` sep

  parseExpression :: Parser Expression
  parseExpression = label "expression" $ do
    op <- assignment <|> makeExprParser parseTerm opTable
    return op

  parseTerm :: Parser Expression
  parseTerm = baseTerm >>= (leftTerm choices)
    where choices op = selector op <|> ifGuard op <|> unlessGuard op <|> invoke op

  parseLeftHandTerm :: Parser Expression
  parseLeftHandTerm = baseTerm >>= (leftTerm choices)
    where choices op = selector op <|> ifGuard op <|> unlessGuard op

  leftTerm :: (Expression -> Parser Expression) -> Expression -> Parser Expression
  leftTerm list op = do
    ex <- optional (list op)
    case ex of
      Nothing -> return op
      Just x -> leftTerm list x

  baseTerm :: Parser Expression
  baseTerm =  primaryValue

  primaryValue :: Parser Expression
  primaryValue = defined
           <|> parseMethod
           <|> parseModule
           <|> parseClass
           <|> begin
           <|> self
           <|> super
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
           <|> parseConstant
           <|> localVariable
           <|> parens parseExpression
           <|> parseLiteral
