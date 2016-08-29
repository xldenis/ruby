{-# LANGUAGE DuplicateRecordFields, OverloadedLabels #-}
module Ruby.AST where

  type Name = String

  data Rescue = Rescue { exception :: Name, body :: Expression }

  data CondBranch
    = ElseIf { condition :: Expression, expression :: Expression, branch :: CondBranch  }
    | Else { expression :: Expression }
    | Nil
    deriving (Show)

  data CaseBranch
    = When { condition :: Expression, expression :: Expression }
    | DefaultCase { expression :: Expression }
    deriving (Show)

  data Arg
    = Basic { name :: Name, defaultVal :: Maybe Expression }
    | Keyword { name :: Name, defaultVal :: Maybe Expression }
    | ArraySplat { name :: Name }
    | HashSplat { name :: Name }
    deriving (Show)

  data BinaryOp
    = Minus
    deriving (Show)

  data ConstantName
   = Namespace Name ConstantName
   | Name Name
   deriving (Show)

  data Expression
    = Begin { body :: Expression, rescue :: Maybe Expression, ensure :: Maybe Expression, elseBlock :: Maybe Expression }
    | Module { name :: ConstantName, expression :: Expression }
    | Class { name :: ConstantName, superClass :: Maybe ConstantName, expression :: Expression }
    | Seq { expressions :: [Expression] }
    | Alias { target :: Name, source :: Name }
    | Method { method :: Name, args :: [Arg], expression :: Expression }
    | Undefine { method :: Name }
    | Defined { expression :: Expression }
    | Block { lhs :: [Name], expression :: Expression }
    | Redo
    | Retry
    | Break
    | Return
    | Next
    | Yield
    | Raise
    | Splat { expression :: Expression }
    | For { names :: [Name], expression :: Expression }
    | Until { condition :: Expression, expression :: Expression }
    | While { condition :: Expression, expression :: Expression }
    | IfMod { expression :: Expression, condition :: Expression }
    | UnlessMod { expression :: Expression, condition :: Expression }
    | If { condition :: Expression, branch :: CondBranch }
    | Unless { condition :: Expression, branch :: CondBranch }
    | Case { condition :: Expression, clause :: CaseBranch }
    | And { left :: Expression, right :: Expression }
    | Or { left :: Expression, right :: Expression }
    | Not { right :: Expression }
    | Dot { object :: Expression, method :: Name }
    | Self
    | Invoke
    | BinaryOp { op :: BinaryOp, left :: Expression, right :: Expression }
    | Assign
    deriving (Show)
