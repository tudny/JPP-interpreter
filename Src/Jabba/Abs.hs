-- File generated by the BNF Converter (bnfc 2.9.4.1).

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The abstract syntax of language jabba.

module Src.Jabba.Abs where

import Prelude (Integer, String)
import qualified Prelude as C
  ( Eq, Ord, Show, Read
  , Functor, Foldable, Traversable
  , Int, Maybe(..)
  )
import qualified Data.String

type Program = Program' BNFC'Position
data Program' a = PProgram a [Instr' a]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Instr = Instr' BNFC'Position
data Instr' a
    = DFun a Ident [Arg' a] (Type' a) (Block' a)
    | DFunUnit a Ident [Arg' a] (Block' a)
    | IUnit a
    | IIncr a Ident
    | IDecr a Ident
    | IAss a Ident (Expr' a)
    | IRet a (Expr' a)
    | IRetUnit a
    | IBreak a
    | ICont a
    | IIf a (IfStmt' a)
    | IWhile a (Expr' a) (Block' a)
    | IWhileFin a (Expr' a) (Block' a) (Block' a)
    | IFor a Ident (Expr' a) (Expr' a) (Block' a)
    | IExpr a (Expr' a)
    | IDecl a (Decl' a)
    | IBBlock a (Block' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Arg = Arg' BNFC'Position
data Arg' a
    = RefMutArg a Ident (Type' a)
    | RefConstArg a Ident (Type' a)
    | CopyMutArg a Ident (Type' a)
    | CopyConstArg a Ident (Type' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Item = Item' BNFC'Position
data Item' a
    = DItemVal a Ident (Type' a) (Expr' a)
    | DItemAuto a Ident (Expr' a)
    | DItem a Ident (Type' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Decl = Decl' BNFC'Position
data Decl' a = DVar a [Item' a] | DVal a [Item' a]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Block = Block' BNFC'Position
data Block' a = IBlock a [Instr' a]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type IfStmt = IfStmt' BNFC'Position
data IfStmt' a
    = IfIf a (Expr' a) (Block' a)
    | IfElse a (Expr' a) (Block' a) (Block' a)
    | IfElseIf a (Expr' a) (Block' a) (IfStmt' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Type = Type' BNFC'Position
data Type' a
    = TInt a
    | TBool a
    | TString a
    | TVoid a
    | TFun a [TArg' a] (Type' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type TArg = TArg' BNFC'Position
data TArg' a
    = TRefMutArg a (Type' a)
    | TRefConstArg a (Type' a)
    | TCopyMutArg a (Type' a)
    | TCopyConstArg a (Type' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type PlsOp = PlsOp' BNFC'Position
data PlsOp' a = OPlus a | OMinus a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type MulOp = MulOp' BNFC'Position
data MulOp' a = OMul a | ODiv a | OMod a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type NotOp = NotOp' BNFC'Position
data NotOp' a = ONot a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type NegOp = NegOp' BNFC'Position
data NegOp' a = ONeg a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type AndOp = AndOp' BNFC'Position
data AndOp' a = OAnd a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type OrOp = OrOp' BNFC'Position
data OrOp' a = OOr a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type RelOp = RelOp' BNFC'Position
data RelOp' a = REq a | RNeq a | RLt a | RGt a | RLeq a | RGeq a
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

type Expr = Expr' BNFC'Position
data Expr' a
    = EVarName a Ident
    | EIntLit a Integer
    | EBoolLitTrue a
    | EBoolLitFalse a
    | EUnitLiteral a
    | EStringLit a String
    | ERun a (Expr' a) [Expr' a]
    | ELambda a [Arg' a] (Block' a)
    | ELambdaEmpty a (Block' a)
    | ENeg a (NegOp' a) (Expr' a)
    | ENot a (NotOp' a) (Expr' a)
    | EMul a (Expr' a) (MulOp' a) (Expr' a)
    | ESum a (Expr' a) (PlsOp' a) (Expr' a)
    | ERel a (Expr' a) (RelOp' a) (Expr' a)
    | EBAnd a (Expr' a) (AndOp' a) (Expr' a)
    | EBOr a (Expr' a) (OrOp' a) (Expr' a)
    | ETer a (Expr' a) (Expr' a) (Expr' a)
    | ELambdaExpr a [Arg' a] (Expr' a)
    | ELambdaEmptEpr a (Expr' a)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Functor, C.Foldable, C.Traversable)

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

-- | Start position (line, column) of something.

type BNFC'Position = C.Maybe (C.Int, C.Int)

pattern BNFC'NoPosition :: BNFC'Position
pattern BNFC'NoPosition = C.Nothing

pattern BNFC'Position :: C.Int -> C.Int -> BNFC'Position
pattern BNFC'Position line col = C.Just (line, col)

-- | Get the start position of something.

class HasPosition a where
  hasPosition :: a -> BNFC'Position

instance HasPosition Program where
  hasPosition = \case
    PProgram p _ -> p

instance HasPosition Instr where
  hasPosition = \case
    DFun p _ _ _ _ -> p
    DFunUnit p _ _ _ -> p
    IUnit p -> p
    IIncr p _ -> p
    IDecr p _ -> p
    IAss p _ _ -> p
    IRet p _ -> p
    IRetUnit p -> p
    IBreak p -> p
    ICont p -> p
    IIf p _ -> p
    IWhile p _ _ -> p
    IWhileFin p _ _ _ -> p
    IFor p _ _ _ _ -> p
    IExpr p _ -> p
    IDecl p _ -> p
    IBBlock p _ -> p

instance HasPosition Arg where
  hasPosition = \case
    RefMutArg p _ _ -> p
    RefConstArg p _ _ -> p
    CopyMutArg p _ _ -> p
    CopyConstArg p _ _ -> p

instance HasPosition Item where
  hasPosition = \case
    DItemVal p _ _ _ -> p
    DItemAuto p _ _ -> p
    DItem p _ _ -> p

instance HasPosition Decl where
  hasPosition = \case
    DVar p _ -> p
    DVal p _ -> p

instance HasPosition Block where
  hasPosition = \case
    IBlock p _ -> p

instance HasPosition IfStmt where
  hasPosition = \case
    IfIf p _ _ -> p
    IfElse p _ _ _ -> p
    IfElseIf p _ _ _ -> p

instance HasPosition Type where
  hasPosition = \case
    TInt p -> p
    TBool p -> p
    TString p -> p
    TVoid p -> p
    TFun p _ _ -> p

instance HasPosition TArg where
  hasPosition = \case
    TRefMutArg p _ -> p
    TRefConstArg p _ -> p
    TCopyMutArg p _ -> p
    TCopyConstArg p _ -> p

instance HasPosition PlsOp where
  hasPosition = \case
    OPlus p -> p
    OMinus p -> p

instance HasPosition MulOp where
  hasPosition = \case
    OMul p -> p
    ODiv p -> p
    OMod p -> p

instance HasPosition NotOp where
  hasPosition = \case
    ONot p -> p

instance HasPosition NegOp where
  hasPosition = \case
    ONeg p -> p

instance HasPosition AndOp where
  hasPosition = \case
    OAnd p -> p

instance HasPosition OrOp where
  hasPosition = \case
    OOr p -> p

instance HasPosition RelOp where
  hasPosition = \case
    REq p -> p
    RNeq p -> p
    RLt p -> p
    RGt p -> p
    RLeq p -> p
    RGeq p -> p

instance HasPosition Expr where
  hasPosition = \case
    EVarName p _ -> p
    EIntLit p _ -> p
    EBoolLitTrue p -> p
    EBoolLitFalse p -> p
    EUnitLiteral p -> p
    EStringLit p _ -> p
    ERun p _ _ -> p
    ELambda p _ _ -> p
    ELambdaEmpty p _ -> p
    ENeg p _ _ -> p
    ENot p _ _ -> p
    EMul p _ _ _ -> p
    ESum p _ _ _ -> p
    ERel p _ _ _ -> p
    EBAnd p _ _ _ -> p
    EBOr p _ _ _ -> p
    ETer p _ _ _ -> p
    ELambdaExpr p _ _ -> p
    ELambdaEmptEpr p _ -> p

