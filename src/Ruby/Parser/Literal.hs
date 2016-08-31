module Ruby.Parser.Literal where
  import Ruby.Parser.Lexer
  import Ruby.AST

  import Text.Megaparsec.Char  as C
  import Text.Megaparsec
  import Text.Megaparsec.Text

  parseLiteral :: Parser Expression
  parseLiteral = lexeme $ hexadecimal <|> binary <|> integer

  underscored :: Parser a -> Parser [a]
  underscored c = c `sepBy` char '_'

  integer :: Parser Expression
  integer = do
    intString <- concat <$> underscored (some C.digitChar)

    return $ Integer Decimal (read intString)

  hexadecimal :: Parser Expression
  hexadecimal = (Integer Hexadecimal . read . concat) <$> (string "0x" *> underscored (some C.hexDigitChar))

  binary :: Parser Expression
  binary = (Integer Binary . read . concat) <$> (string "0b" *> underscored (some binaryChar))
    where binaryChar = satisfy (\c -> c == '0' || c == '1')

  octal = do
    char '0'
    optional (char '_')
    underscored (some C.octDigitChar)


