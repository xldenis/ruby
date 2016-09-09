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
    unitSpec
    filesShouldParse "test/parser/success/expression" (parseProgram)
    filesShouldParse "test/parser/success/literal" (some $ parseLiteral <* newline)
    filesShouldParse "test/parser/success/advanced" (parseProgram)
    filesShouldFail "test/parser/failure" (parseProgram)

  unitSpec :: Spec
  unitSpec = do
    describe "assignment parses" $ do
      it "single assignment" $ do
        parse (assignment <* eof) "" "self = 1" `shouldBe` (Right $ Assign [Self] [Literal $ Integer Decimal 1])
      it "double left assignment" $ do
        parse (assignment <* eof) "" "self, self = 1" `shouldBe` (Right $ Assign [Self, Self] [Literal $ Integer Decimal 1])
      it "double left assignment" $ do
        parse (assignment <* eof) "" "self = 1, 1" `shouldBe` (Right $ Assign [Self] [Literal $ Integer Decimal 1, Literal $ Integer Decimal 1])
    describe "invoke parses" $ do
      it "single arguments" $ do
        parse (invoke (Super) <* eof) "" "2" `shouldBe` (Right $ (Super) `Invoke` [Normal . Literal $ Integer Decimal 2])

