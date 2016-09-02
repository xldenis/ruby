module Ruby.Parser.Literal where
  import Ruby.Parser.Lexer
  import Ruby.AST

  import Data.Maybe
  import Data.Text (pack)

  import Text.Megaparsec.Char  as C
  import Text.Megaparsec
  import Text.Megaparsec.Text

  parseLiteral :: Parser Expression
  parseLiteral = singleString <|> symbolString <|> intLit

  intLit :: Parser Expression
  intLit = lexeme $ hexadecimal <|> binary <|> octal <|> decimalAndOctal

  underscored :: Parser a -> Parser [a]
  underscored c = c `sepBy1` char '_'

  decimalAndOctal :: Parser Expression
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

  hexadecimal :: Parser Expression
  hexadecimal = (Integer Hexadecimal . read . ("0x" ++) . concat) <$> (string' "0x" *> underscored (some C.hexDigitChar))

  binary :: Parser Expression
  binary = (Integer Binary . read . concat) <$> (string "0b" *> underscored (some binaryChar))
    where binaryChar = satisfy (\c -> c == '0' || c == '1')

  octal :: Parser Expression
  octal = (Integer Octal . read . ("0o" ++) . concat) <$> (string' "0o" *> underscored (some C.octDigitChar))

  singleString :: Parser Expression
  singleString = lexeme $ do
    strSegments <- squotes $ many stringChar

    return . RawString . pack $ concat strSegments
    where stringChar = (string "\\'" *> return "\\'") <|> (flip (:) [] <$> satisfy ('\'' /=))

  symbolString :: Parser Expression
  symbolString = lexeme $ (Symbol . pack) <$> (char ':' *> symbolIdentifier)
