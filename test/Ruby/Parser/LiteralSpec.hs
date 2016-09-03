{-# LANGUAGE OverloadedStrings #-}
module Ruby.Parser.LiteralSpec where
  import Test.Hspec
  import SpecHelper

  import Text.Megaparsec (some, many, parse, eof)
  import Text.Megaparsec.Char (newline)

  import Ruby.AST
  import Ruby.Parser.Literal


  spec :: Spec
  spec = parallel $ do
    filesShouldParse "test/parser/success/literal" (many $ parseLiteral <* newline)
    unitSpec

  unitSpec :: Spec
  unitSpec = do
    describe "parseLiteral" $ do
      it "prefers octal over decimal" $ do
        parse (intLit <* eof) "" "0377" `shouldBe` (Right $ Integer Octal 255)
      it "parses decimal when not valid octal" $ do
        parse (intLit <* eof) "" "0387" `shouldBe` (Right $ Integer Decimal 387)
      it "parses hex" $ do
        parse (intLit <* eof) "" "0xFF" `shouldBe` (Right $ Integer Hexadecimal 255)
      it "parses octal" $ do
        parse (intLit <* eof) "" "0o55" `shouldBe` (Right $ Integer Octal 45)
    describe "singleString" $ do
      it "parses" $ do
        parse (singleString <* eof) "" "'test'" `shouldBe` (Right $ String "test")
