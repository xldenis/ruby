module Ruby.Parser.Operator where
  import Text.Megaparsec
  import Text.Megaparsec.Text
  import Text.Megaparsec.Expr

  import Ruby.Parser.Lexer (symbol)

  import Ruby.AST (BinaryOp(..), Expression(BinaryOp))

  opTable :: [[Operator Parser Expression]]
  opTable = [ [ binary "**" Exponent ]
            , [ binary "~"  BitFlip

              ]
            , [ binary "*" Multiplication
              , binary "/" Division
              , binary "%" Modulus
              ]
            , [ binary "+"  Plus
              , binary "-"  Minus
              ]
            , [ binary ">>" RightShift
              , binary "<<" LeftShift
              ]
            , [ binary "&" BitAnd ]
            , [ binary "^" BitXor
              , binary "|" BitOr
              ]
            , [ binary "<=" LessEqual
              , binary "<"  LessThan
              , binary ">"  GreaterThan
              , binary ">=" GreaterEqual
              ]
            , [ binary "<=>" Compare
              , binary "=="  Equal
              , binary "===" ThreeEqual
              , binary "!="  NotEqual
              , binary "=~"  Match
              , binary "!~"  NotMatch
              ]
            , [ binary "&&" And ]
            , [ binary "||" Or ]
            , [ binary ".." InclusiveRange
              , binary "..." ExclusiveRange
              ]
            , [ binary "or" AndWord
              , binary "and" AndWord
              ]
            ]

  binary :: String -> BinaryOp -> Operator Parser Expression
  binary op cons = InfixL $ do
    op <- symbol op *> return cons
    return $ \a b -> BinaryOp op a b
