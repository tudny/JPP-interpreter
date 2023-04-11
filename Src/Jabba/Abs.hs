-- File generated by the BNF Converter (bnfc 2.9.4.1).

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language jabba.

module Src.Jabba.Abs where

import Prelude (Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

data Program = PProgram [Instr]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Instr
    = DFun Ident [Arg] Type Block
    | IUnit
    | IIncr Ident
    | IDecr Ident
    | IAss Ident Expr
    | IRet Expr
    | IRetUnit
    | IYield Expr
    | IYieldUnit
    | IIf Expr Block
    | IIfElse Expr Block Block
    | IWhile Expr Block
    | IWhileFin Expr Block Block
    | IFor Ident Expr Expr Block
    | IForGen Ident Expr Block
    | IExpr Expr
    | IDecl Decl
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Arg
    = RefMutArg Ident Type
    | RefConstArg Ident Type
    | CopyMutArg Ident Type
    | CopyConstArg Ident Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Item = DItemVal Ident Type Expr | DItem Ident Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Decl = DVar [Item] | DVal [Item]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Block = IBlock [Instr]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Type = TInt | TBool | TString | TGen Type | TVoid
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data PlsOp = OPlus | OMinus
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data MulOp = OMul | ODiv | OMod
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data NotOp = ONot
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data NegOp = ONeg
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data AndOp = OAnd
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data OrOp = OOr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data RelOp = REq | RNeq | RLt | RGt | RLeq | RGeq
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Expr
    = EVarName Ident
    | EIntLit Integer
    | EBoolLitTrue
    | EBoolLitFalse
    | ERun Ident [Expr]
    | EStringLit String
    | ENeg NegOp Expr
    | ENot NotOp Expr
    | EMul Expr MulOp Expr
    | ESum Expr PlsOp Expr
    | ERel Expr RelOp Expr
    | EBAnd Expr AndOp Expr
    | EBOr Expr OrOp Expr
    | ETer Expr Expr Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)
