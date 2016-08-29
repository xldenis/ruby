module Ruby.ParserSpec where

  import Ruby.Parser

  import Test.Hspec

  import SpecHelper

  spec :: Spec
  spec = do
    filesShouldParse "test/parser/success" parseExpressions
