module Ruby.Parser.Literal where
  import Ruby.Parser.Lexer
  import Ruby.AST

  import Data.Maybe
  import Data.Text (pack)

  import Text.Megaparsec.Char  as C
  import Text.Megaparsec
  import Text.Megaparsec.Text

  parseLiteral :: Parser Expression
  parseLiteral = Literal <$> (choice [singleString, symbolString, intLit, parseBoolean, nil])

  nil :: Parser Literal
  nil = symbol "nil" *> return Nil

  parseBoolean :: Parser Literal
  parseBoolean = symbol "false" *> (return $ Boolean False)

  intLit :: Parser Literal
  intLit = lexeme $ hexadecimal <|> binary <|> octal <|> decimalAndOctal

  underscored :: Parser a -> Parser [a]
  underscored c = c `sepBy1` char '_'

  decimalAndOctal :: Parser Literal
  decimalAndOctal = do
    intString <- concat <$> underscored (some C.digitChar)
    let isOctal = case listToMaybe intString of
                    Just head -> head == '0' && octString intString
                    Nothing   -> False
    if isOctal then
      return $ Integer Octal (read $ "0o" ++ intString)
    else
      return $ Integer Decimal (read intString)
    where octString a = not $ any (\c -> c == '8' || c == '9') a

  hexadecimal :: Parser Literal
  hexadecimal = (Integer Hexadecimal . read . ("0x" ++) . concat) <$> (string' "0x" *> underscored (some C.hexDigitChar))

  binary :: Parser Literal
  binary = (Integer Binary . read . concat) <$> (string "0b" *> underscored (some binaryChar))
    where binaryChar = satisfy (\c -> c == '0' || c == '1')

  octal :: Parser Literal
  octal = (Integer Octal . read . ("0o" ++) . concat) <$> (string' "0o" *> underscored (some C.octDigitChar))

  singleString :: Parser Literal
  singleString = lexeme $ do
    strSegments <- squotes $ many stringChar

    return . String . pack $ concat strSegments
    where stringChar = (string "\\'" *> return "\\'") <|> (flip (:) [] <$> satisfy ('\'' /=))

  symbolString :: Parser Literal
  symbolString = lexeme $ (Symbol . pack) <$> (char ':' *> symbolIdentifier)
