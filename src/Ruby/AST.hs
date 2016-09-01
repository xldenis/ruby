{-# LANGUAGE DuplicateRecordFields, OverloadedLabels #-}
module Ruby.AST where
  import Data.Text

  type Name = String

  data CondBranch
    = ElseIf { condition :: Expression, expression :: Expression, branch :: CondBranch  }
    | Else { expression :: Expression }
    | Nil
    deriving (Show, Eq)

  data CaseBranch
    = When { condition :: Expression, expression :: Expression }
    | DefaultCase { expression :: Expression }
    deriving (Show, Eq)

  data Arg
    = Basic { name :: Name, defaultVal :: Maybe Expression }
    | Keyword { name :: Name, defaultVal :: Maybe Expression }
    | ArraySplat { name :: Name }
    | HashSplat { name :: Name }
    deriving (Show, Eq)

  data BinaryOp
    = Minus
    | OrWord
    | AndWord
    | Or
    | And
    deriving (Show, Eq)

  data ConstantName
   = Namespace Name ConstantName
   | Name Name
   deriving (Show, Eq)

  data IntType
    = Binary
    | Octal
    | Decimal
    | Hexadecimal
    deriving (Show, Eq)

  data Rescue
    = ExceptionClasses { classes :: [Expression], expression :: Expression }
    | ExceptionBinding { classes :: [Expression], variable :: Name, expression :: Expression}
    deriving (Show, Eq)

  data Expression
    = Begin { body :: Expression, rescue :: [Rescue], ensure :: Maybe Expression, elseBlock :: Maybe Expression }
    | Module { name :: ConstantName, expression :: Expression }
    | Class { name :: ConstantName, superClass :: Maybe ConstantName, expression :: Expression }
    | Seq { expressions :: [Expression] }
    | Alias { target :: Name, source :: Name }
    | Method { method :: Name, args :: [Arg], expression :: Expression }
    | Undefine { methods :: [Name] }
    | Defined { expression :: Expression }
    | Block { lhs :: [Name], expression :: Expression }
    | InlineBlock { lhs :: [Name], expression :: Expression }
    | Redo
    | Retry
    | Break  { expressions :: [Expression] }
    | Return { expressions :: [Expression] }
    | Next   { expressions :: [Expression] }
    | Yield  { expressions :: [Expression] }
    | Raise { expression :: Expression }
    | Splat { expression :: Expression }
    | For { lhs :: [Name], expression :: Expression }
    | Until { condition :: Expression, expression :: Expression }
    | While { condition :: Expression, expression :: Expression }
    | IfMod { expression :: Expression, condition :: Expression }
    | UnlessMod { expression :: Expression, condition :: Expression }
    | If { condition :: Expression, branch :: CondBranch }
    | Unless { condition :: Expression, branch :: CondBranch }
    | Case { condition :: Expression, clause :: CaseBranch }
    | Not { right :: Expression }
    | Dot { object :: Expression, method :: Name }
    | Self
    | Invoke
    | BinaryOp { op :: BinaryOp, left :: Expression, right :: Expression }
    | Assign { lhs :: [Name], rhs :: [Expression]}
    | Integer { kind :: IntType, value :: Integer }
    | RawString { strVal :: Text }
    | Symbol    { strVal :: Text }
    | Require { expression :: Expression }
    deriving (Show, Eq)
