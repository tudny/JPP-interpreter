-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.4.1).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Src.Jabba.Par
  ( happyError
  , myLexer
  , pProgram
  ) where

import Prelude

import qualified Src.Jabba.Abs
import Src.Jabba.Lex

}

%name pProgram_internal Program
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '!'        { PT _ (TS _ 1)  }
  '!='       { PT _ (TS _ 2)  }
  '$'        { PT _ (TS _ 3)  }
  '%'        { PT _ (TS _ 4)  }
  '&&'       { PT _ (TS _ 5)  }
  '('        { PT _ (TS _ 6)  }
  ')'        { PT _ (TS _ 7)  }
  '*'        { PT _ (TS _ 8)  }
  '+'        { PT _ (TS _ 9)  }
  '++'       { PT _ (TS _ 10) }
  ','        { PT _ (TS _ 11) }
  '-'        { PT _ (TS _ 12) }
  '--'       { PT _ (TS _ 13) }
  '->'       { PT _ (TS _ 14) }
  '..'       { PT _ (TS _ 15) }
  '/'        { PT _ (TS _ 16) }
  ':'        { PT _ (TS _ 17) }
  ';'        { PT _ (TS _ 18) }
  '<'        { PT _ (TS _ 19) }
  '<='       { PT _ (TS _ 20) }
  '='        { PT _ (TS _ 21) }
  '=='       { PT _ (TS _ 22) }
  '>'        { PT _ (TS _ 23) }
  '>='       { PT _ (TS _ 24) }
  '?'        { PT _ (TS _ 25) }
  'Boolean'  { PT _ (TS _ 26) }
  'Integer'  { PT _ (TS _ 27) }
  'String'   { PT _ (TS _ 28) }
  'Unit'     { PT _ (TS _ 29) }
  '['        { PT _ (TS _ 30) }
  ']'        { PT _ (TS _ 31) }
  'break'    { PT _ (TS _ 32) }
  'continue' { PT _ (TS _ 33) }
  'else'     { PT _ (TS _ 34) }
  'false'    { PT _ (TS _ 35) }
  'finally'  { PT _ (TS _ 36) }
  'for'      { PT _ (TS _ 37) }
  'fun'      { PT _ (TS _ 38) }
  'if'       { PT _ (TS _ 39) }
  'new'      { PT _ (TS _ 40) }
  'return'   { PT _ (TS _ 41) }
  'true'     { PT _ (TS _ 42) }
  'unit'     { PT _ (TS _ 43) }
  'val'      { PT _ (TS _ 44) }
  'var'      { PT _ (TS _ 45) }
  'while'    { PT _ (TS _ 46) }
  '{'        { PT _ (TS _ 47) }
  '|'        { PT _ (TS _ 48) }
  '||'       { PT _ (TS _ 49) }
  '}'        { PT _ (TS _ 50) }
  L_Ident    { PT _ (TV _)    }
  L_integ    { PT _ (TI _)    }
  L_quoted   { PT _ (TL _)    }

%%

Ident :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.Ident) }
Ident  : L_Ident { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.Ident (tokenText $1)) }

Integer :: { (Src.Jabba.Abs.BNFC'Position, Integer) }
Integer  : L_integ  { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), (read (tokenText $1)) :: Integer) }

String  :: { (Src.Jabba.Abs.BNFC'Position, String) }
String   : L_quoted { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), ((\(PT _ (TL s)) -> s) $1)) }

Program :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.Program) }
Program
  : ListInstr { (fst $1, Src.Jabba.Abs.PProgram (fst $1) (snd $1)) }

Instr :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.Instr) }
Instr
  : 'fun' Ident '(' ListArg ')' ':' Type Block { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.DFun (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $4) (snd $7) (snd $8)) }
  | 'fun' Ident '(' ListArg ')' Block { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.DFunUnit (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $4) (snd $6)) }
  | ';' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.IUnit (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }
  | Ident '++' ';' { (fst $1, Src.Jabba.Abs.IIncr (fst $1) (snd $1)) }
  | Ident '--' ';' { (fst $1, Src.Jabba.Abs.IDecr (fst $1) (snd $1)) }
  | Ident '=' Expr ';' { (fst $1, Src.Jabba.Abs.IAss (fst $1) (snd $1) (snd $3)) }
  | 'return' Expr ';' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.IRet (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | 'return' ';' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.IRetUnit (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'break' ';' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.IBreak (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'continue' ';' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.ICont (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }
  | IfStmt { (fst $1, Src.Jabba.Abs.IIf (fst $1) (snd $1)) }
  | 'while' Expr Block { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.IWhile (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $3)) }
  | 'while' Expr Block 'finally' Block { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.IWhileFin (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $3) (snd $5)) }
  | 'for' Ident '=' Expr '..' Expr Block { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.IFor (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $4) (snd $6) (snd $7)) }
  | Expr ';' { (fst $1, Src.Jabba.Abs.IExpr (fst $1) (snd $1)) }
  | Decl ';' { (fst $1, Src.Jabba.Abs.IDecl (fst $1) (snd $1)) }
  | Block { (fst $1, Src.Jabba.Abs.IBBlock (fst $1) (snd $1)) }
  | Ident '[' Expr ']' '=' Expr ';' { (fst $1, Src.Jabba.Abs.ITabAss (fst $1) (snd $1) (snd $3) (snd $6)) }

Arg :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.Arg) }
Arg
  : 'var' Ident ':' Type { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.RefMutArg (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $4)) }
  | 'val' Ident ':' Type { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.RefConstArg (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $4)) }
  | 'var' Ident ':' 'new' Type { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.CopyMutArg (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $5)) }
  | 'val' Ident ':' 'new' Type { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.CopyConstArg (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $5)) }

ListArg :: { (Src.Jabba.Abs.BNFC'Position, [Src.Jabba.Abs.Arg]) }
ListArg
  : {- empty -} { (Src.Jabba.Abs.BNFC'NoPosition, []) }
  | Arg { (fst $1, (:[]) (snd $1)) }
  | Arg ',' ListArg { (fst $1, (:) (snd $1) (snd $3)) }

Item :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.Item) }
Item
  : Ident ':' Type '=' Expr { (fst $1, Src.Jabba.Abs.DItemVal (fst $1) (snd $1) (snd $3) (snd $5)) }
  | Ident '=' Expr { (fst $1, Src.Jabba.Abs.DItemAuto (fst $1) (snd $1) (snd $3)) }
  | Ident ':' Type { (fst $1, Src.Jabba.Abs.DItem (fst $1) (snd $1) (snd $3)) }

ListItem :: { (Src.Jabba.Abs.BNFC'Position, [Src.Jabba.Abs.Item]) }
ListItem
  : {- empty -} { (Src.Jabba.Abs.BNFC'NoPosition, []) }
  | Item { (fst $1, (:[]) (snd $1)) }
  | Item ',' ListItem { (fst $1, (:) (snd $1) (snd $3)) }

Decl :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.Decl) }
Decl
  : 'var' ListItem { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.DVar (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | 'val' ListItem { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.DVal (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }

Block :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.Block) }
Block
  : '{' ListInstr '}' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.IBlock (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }

ListInstr :: { (Src.Jabba.Abs.BNFC'Position, [Src.Jabba.Abs.Instr]) }
ListInstr
  : {- empty -} { (Src.Jabba.Abs.BNFC'NoPosition, []) }
  | Instr ListInstr { (fst $1, (:) (snd $1) (snd $2)) }

IfStmt :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.IfStmt) }
IfStmt
  : 'if' Expr Block { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.IfIf (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $3)) }
  | 'if' Expr Block 'else' Block { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.IfElse (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $3) (snd $5)) }
  | 'if' Expr Block 'else' IfStmt { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.IfElseIf (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $3) (snd $5)) }

Type :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.Type) }
Type
  : 'Integer' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.TInt (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'Boolean' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.TBool (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'String' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.TString (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'Unit' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.TVoid (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }
  | '(' ListTArg ')' '->' Type { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.TFun (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $5)) }
  | '[' Type ']' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.TTab (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }

TArg :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.TArg) }
TArg
  : 'var' Type { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.TRefMutArg (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | 'val' Type { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.TRefConstArg (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | 'var' '$' Type { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.TCopyMutArg (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $3)) }
  | 'val' '$' Type { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.TCopyConstArg (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $3)) }

ListTArg :: { (Src.Jabba.Abs.BNFC'Position, [Src.Jabba.Abs.TArg]) }
ListTArg
  : {- empty -} { (Src.Jabba.Abs.BNFC'NoPosition, []) }
  | TArg { (fst $1, (:[]) (snd $1)) }
  | TArg ',' ListTArg { (fst $1, (:) (snd $1) (snd $3)) }

PlsOp :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.PlsOp) }
PlsOp
  : '+' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.OPlus (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }
  | '-' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.OMinus (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }

MulOp :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.MulOp) }
MulOp
  : '*' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.OMul (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }
  | '/' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.ODiv (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }
  | '%' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.OMod (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }

NotOp :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.NotOp) }
NotOp
  : '!' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.ONot (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }

NegOp :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.NegOp) }
NegOp
  : '-' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.ONeg (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }

AndOp :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.AndOp) }
AndOp
  : '&&' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.OAnd (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }

OrOp :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.OrOp) }
OrOp
  : '||' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.OOr (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }

RelOp :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.RelOp) }
RelOp
  : '==' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.REq (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }
  | '!=' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.RNeq (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }
  | '<' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.RLt (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }
  | '>' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.RGt (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }
  | '<=' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.RLeq (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }
  | '>=' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.RGeq (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }

Expr6 :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.Expr) }
Expr6
  : Ident '[' Expr ']' { (fst $1, Src.Jabba.Abs.ITabAcc (fst $1) (snd $1) (snd $3)) }
  | Ident { (fst $1, Src.Jabba.Abs.EVarName (fst $1) (snd $1)) }
  | Integer { (fst $1, Src.Jabba.Abs.EIntLit (fst $1) (snd $1)) }
  | 'true' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.EBoolLitTrue (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'false' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.EBoolLitFalse (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'unit' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.EUnitLiteral (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1))) }
  | String { (fst $1, Src.Jabba.Abs.EStringLit (fst $1) (snd $1)) }
  | '(' Expr ')' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), (snd $2)) }

Expr :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.Expr) }
Expr
  : '[' Expr ';' Expr ']' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.ITabInit (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $4)) }
  | '[' ListExpr ']' { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.ITabInitEls (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | Expr1 OrOp Expr { (fst $1, Src.Jabba.Abs.EBOr (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr1 '?' Expr1 ':' Expr { (fst $1, Src.Jabba.Abs.ETer (fst $1) (snd $1) (snd $3) (snd $5)) }
  | '|' ListArg '|' Expr1 { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.ELambdaExpr (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $4)) }
  | '||' Expr1 { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.ELambdaEmptEpr (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | Expr1 { (fst $1, (snd $1)) }

Expr5 :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.Expr) }
Expr5
  : Expr5 '(' ListExpr ')' { (fst $1, Src.Jabba.Abs.ERun (fst $1) (snd $1) (snd $3)) }
  | '|' ListArg '|' Block { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.ELambda (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $4)) }
  | '||' Block { (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1), Src.Jabba.Abs.ELambdaEmpty (uncurry Src.Jabba.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | NegOp Expr6 { (fst $1, Src.Jabba.Abs.ENeg (fst $1) (snd $1) (snd $2)) }
  | NotOp Expr6 { (fst $1, Src.Jabba.Abs.ENot (fst $1) (snd $1) (snd $2)) }
  | Expr6 { (fst $1, (snd $1)) }

Expr4 :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.Expr) }
Expr4
  : Expr4 MulOp Expr5 { (fst $1, Src.Jabba.Abs.EMul (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr5 { (fst $1, (snd $1)) }

Expr3 :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.Expr) }
Expr3
  : Expr3 PlsOp Expr4 { (fst $1, Src.Jabba.Abs.ESum (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr4 { (fst $1, (snd $1)) }

Expr2 :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.Expr) }
Expr2
  : Expr2 RelOp Expr3 { (fst $1, Src.Jabba.Abs.ERel (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr3 { (fst $1, (snd $1)) }

Expr1 :: { (Src.Jabba.Abs.BNFC'Position, Src.Jabba.Abs.Expr) }
Expr1
  : Expr2 AndOp Expr1 { (fst $1, Src.Jabba.Abs.EBAnd (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr2 { (fst $1, (snd $1)) }

ListExpr :: { (Src.Jabba.Abs.BNFC'Position, [Src.Jabba.Abs.Expr]) }
ListExpr
  : {- empty -} { (Src.Jabba.Abs.BNFC'NoPosition, []) }
  | Expr { (fst $1, (:[]) (snd $1)) }
  | Expr ',' ListExpr { (fst $1, (:) (snd $1) (snd $3)) }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

-- Entrypoints

pProgram :: [Token] -> Err Src.Jabba.Abs.Program
pProgram = fmap snd . pProgram_internal
}

