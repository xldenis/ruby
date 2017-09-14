module Ruby.Parser.Operator where
import           Text.Megaparsec
import           Text.Megaparsec.Expr

import           Ruby.Parser.Lexer

import           Data.Text

import           Ruby.AST             (BinaryOp (..),
                                       Expression (BinaryOp, UnaryOp),
                                       UnaryOp (..))

opTable :: [[Operator Parser Expression]]
opTable = [ [ prefix "::" GlobalScope ]
          , [ binary "**" Exponent ]
          , [ prefix "~"  BitFlip
            , prefix "+"  Positive
            , prefix "-" Negative
            , prefix "!" Not
            , unTrailedPrefix "&" "&" BlockYield
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
          , [ singOp "&" BitAnd ]
          , [ binary "^" BitXor
            , singOp "|" BitOr
            ]
          , [ unTrailedOp "<=" ">" LessEqual
            , binary ">=" GreaterEqual
            , unTrailedOp "<" "=" LessThan
            , unTrailedOp ">" "=" GreaterThan
            ]
          , [ binary "<=>" Compare
            , unTrailedOp "==" "=" Equal
            , binary "===" ThreeEqual
            , binary "!="  NotEqual
            , binary "=~"  Match
            , binary "!~"  NotMatch
            ]
          , [ binary "&&" And ]
          , [ binary "||" Or ]
          , [ unTrailedOp ".." "." InclusiveRange
            , binary "..." ExclusiveRange
            ]
          , [ prefix "not" NotWord ]
          , [ binary "or" AndWord
            , binary "and" AndWord
            ]
          ]

unTrailedOp :: Text -> Text -> BinaryOp -> Operator Parser Expression
unTrailedOp op trail cons = InfixL . try $ do
  op <- symbol op
  notFollowedBy (symbol trail)
  return $ \a b -> BinaryOp cons a b

singOp :: Text -> BinaryOp -> Operator Parser Expression
singOp op cons = unTrailedOp op op cons

prefix :: Text -> UnaryOp -> Operator Parser Expression
prefix op cons = Prefix $ do
  op <- symbol op *> return cons
  return $ \a -> UnaryOp op a

unTrailedPrefix :: Text -> Text -> UnaryOp -> Operator Parser Expression
unTrailedPrefix op trail cons = Prefix . try $ do
  op <- symbol op
  notFollowedBy (symbol trail)
  return $ \a -> UnaryOp cons a

binary :: Text -> BinaryOp -> Operator Parser Expression
binary op cons = InfixL $ do
  op <- symbol op *> return cons
  return $ \a b -> BinaryOp op a b
