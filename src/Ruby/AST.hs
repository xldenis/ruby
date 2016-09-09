{-# LANGUAGE DuplicateRecordFields, OverloadedLabels #-}
module Ruby.AST where
  import Data.Text

  type Name = String

  data CondBranch
    = Guard { condition :: Expression, expression :: Expression, branch :: CondBranch  }
    | Else { expression :: Expression }
    | End
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
    | BlockArg { name :: Name }
    deriving (Show, Eq)

  data CallArg
    = Normal { expression :: Expression }
    | Named  { name :: Name, expression :: Expression }
    deriving (Show, Eq)

  data BinaryOp
    -- arithmetic operators
    = Plus
    | Minus
    | Multiplication
    | Division
    | Modulus
    | Exponent
    -- comparison operators
    | Equal
    | NotEqual
    | GreaterThan
    | LessThan
    | GreaterEqual
    | LessEqual
    | Compare
    | ThreeEqual
    -- pattern match
    | Match
    | NotMatch
    -- bitwise operators
    | BitAnd
    | BitOr
    | BitXor
    | LeftShift
    | RightShift
    -- logical operators
    | OrWord
    | AndWord
    | Or
    | And
    -- range
    | InclusiveRange
    | ExclusiveRange
    deriving (Show, Eq)

  data UnaryOp
    = Positive
    | Negative
    | BitFlip
    | GlobalScope
    | Not
    | NotWord
    | BlockYield
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

  data VariableKind
    = Local
    | Instance
    | ClassInstance
    deriving (Show, Eq)

  data Rescue
    = ExceptionClasses { classes :: [Expression], expression :: Expression }
    | ExceptionBinding { classes :: [Expression], variable :: Name, expression :: Expression}
    deriving (Show, Eq)

  data Literal
    = Integer { kind :: IntType, integer :: Integer }
    | Boolean { boolean :: Bool }
    | String { strVal :: Text }
    | Symbol { strVal :: Text }
    | Nil
    deriving (Show, Eq)

  data Expression
    = Begin { body :: Expression, rescue :: [Rescue], ensure :: Maybe Expression, elseBlock :: Maybe Expression }
    | Alias { target :: Name, source :: Name }
    | Assign { assignees :: [Expression], values :: [Expression]}
    | BinaryOp { op :: BinaryOp, left :: Expression, right :: Expression }
    | Block { lhs :: [Name], expression :: Expression }
    | Break  { expressions :: [Expression] }
    | Class { name :: ConstantName, superClass :: Maybe Expression, expression :: Expression }
    | Constant { constant :: ConstantName }
    | Defined { expression :: Expression }
    | Dot { object :: Expression, method :: Name }
    | If { branch :: CondBranch }
    | IfMod { expression :: Expression, condition :: Expression }
    | InlineBlock { lhs :: [Name], expression :: Expression }
    | Invoke { object :: Expression, arguments :: [CallArg] }
    | Literal { value :: Literal }
    | Method { method :: Name, args :: [Arg], expression :: Expression, prefix :: Maybe Expression }
    | Module { name :: ConstantName, expression :: Expression }
    | Next { expressions :: [Expression] }
    | Raise { expression :: Expression }
    | Redo
    | Require { expression :: Expression }
    | Retry
    | Return { expressions :: [Expression] }
    | Self
    | Seq { expressions :: [Expression] }
    | Super
    | UnaryOp { unaryOp :: UnaryOp, expression :: Expression }
    | Undefine { methods :: [Name] }
    | Unless { branch :: CondBranch }
    | UnlessMod { expression :: Expression, condition :: Expression }
    | Variable { varKind :: VariableKind, variable :: Name }
    | Yield  { expressions :: [Expression] }
    -- NOT YET PARSED
    | Encoding
    | Line
    | File
    | EigenClass { expression :: Expression }
    | Splat { expression :: Expression }
    | For { lhs :: [Name], expression :: Expression }
    | Until { condition :: Expression, expression :: Expression }
    | While { condition :: Expression, expression :: Expression }
    | Case { condition :: Expression, clause :: CaseBranch }

    deriving (Show, Eq)
