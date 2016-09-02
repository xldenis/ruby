module Ruby.ParserSpec where
  import Test.Hspec
  import SpecHelper

  import Text.Megaparsec (some, eof, parse)
  import Text.Megaparsec.Char (newline)

  import Ruby.AST
  import Ruby.Parser
  import Ruby.Parser.Literal

  spec :: Spec
  spec = parallel $do
    filesShouldParse "test/parser/success/expression" (parseProgram)
    filesShouldParse "test/parser/success/literal" (some $ parseLiteral <* newline)
    unitSpec

  unitSpec :: Spec
  unitSpec = do
    describe "assignment parses" $ do
      it "single assignment" $ do
        parse (assignment <* eof) "" "a = 1" `shouldBe` (Right $ Assign ["a"] [Integer Decimal 1])
      it "double left assignment" $ do
        parse (assignment <* eof) "" "a, b = 1" `shouldBe` (Right $ Assign ["a", "b"] [Integer Decimal 1])
      it "double left assignment" $ do
        parse (assignment <* eof) "" "a = 1, 1" `shouldBe` (Right $ Assign ["a"] [Integer Decimal 1, Integer Decimal 1])
    describe "invoke parses" $ do
      it "single arguments" $ do
        parse (invoke (Integer Decimal 1) <* eof) "" "2" `shouldBe` (Right $ Integer Decimal 1 `Invoke` [Integer Decimal 2])
