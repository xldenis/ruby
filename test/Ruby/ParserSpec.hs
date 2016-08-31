module Ruby.ParserSpec where
  import Test.Hspec
  import SpecHelper

  import Text.Megaparsec (some)
  import Text.Megaparsec.Char (newline)

  import Ruby.Parser
  import Ruby.Parser.Literal

  spec :: Spec
  spec = do
    filesShouldParse "test/parser/success/expression" parseExpressions
    filesShouldParse "test/parser/success/literal" (some $ parseLiteral <* newline)
