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
    filesShouldParse "test/parser/success/advanced" (parseProgram)
    unitSpec

  unitSpec :: Spec
  unitSpec = do
    describe "assignment parses" $ do
      it "single assignment" $ do
        parse (assignment <* eof) "" "a = 1" `shouldBe` (Right $ Assign ["a"] [Literal $ Integer Decimal 1])
      it "double left assignment" $ do
        parse (assignment <* eof) "" "a, b = 1" `shouldBe` (Right $ Assign ["a", "b"] [Literal $ Integer Decimal 1])
      it "double left assignment" $ do
        parse (assignment <* eof) "" "a = 1, 1" `shouldBe` (Right $ Assign ["a"] [Literal $ Integer Decimal 1, Literal $ Integer Decimal 1])
    describe "invoke parses" $ do
      it "single arguments" $ do
        parse (invoke (Literal $ Integer Decimal 1) <* eof) "" "2" `shouldBe` (Right $ (Literal $ Integer Decimal 1) `Invoke` [Literal $ Integer Decimal 2])
