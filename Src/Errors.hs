module Src.Errors where

import Src.Jabba.Abs
import Src.Types

type Err = Either ErrHolder

data ErrHolder 
    = ParserErr String
    | TypeChecker BNFC'Position ErrType

data ErrType
    = ImmutVar Ident
    | NotDeclVar Ident
    | NotDeclFun Ident
    | WrongType Ident VarType [VarType]
    | WrongTypeArg Int VarType VarType
    | VarAlreadyDecl Ident
    | WrongNumberOfArgs Ident Int Int
    | ExprMutPass Int
    | ImmutMutPass Int
    deriving (Eq)


showPos :: BNFC'Position -> String
showPos BNFC'NoPosition = "unknown position"
showPos (BNFC'Position l c) = "line " ++ show l ++ ", column " ++ show c
showPos pos = show pos


showI :: Ident -> String
showI (Ident s) = "\"" ++ s ++ "\""


instance Show ErrHolder where
    show (TypeChecker pos errT) = 
        "There is a problem at " ++ showPos pos ++
        ": " ++ show errT
    show (ParserErr err) = 
        "There was a parsing problem " ++ 
        show err


instance Show ErrType where
    show (ImmutVar i) = "variable " ++ showI i ++ " is immutable."
    show (NotDeclVar i) = "variable " ++ showI i ++ " was not declared in current scope."
    show (NotDeclFun i) = "function" ++ showI i ++ " was not declared in current scope."
    show (WrongType i t ex) = "variable " ++ showI i ++ " is of type " ++ show t ++ " but expected " ++ show ex ++ "."
    show (WrongTypeArg i t ex) = "argument " ++ show (i + 1) ++ " is of type " ++ show t ++ " but expected " ++ show ex ++ "."
    show (VarAlreadyDecl i) = "variable " ++ showI i ++ " was already declared in declaration block."
    show (WrongNumberOfArgs i ex got) = "function " ++ showI i ++ " was called with " ++ show got ++ " arguments but expected " ++ show ex ++ " arguments."
    show (ExprMutPass i) = "expression " ++ show (i + 1) ++ " was passed to a mutable reference argument."
    show (ImmutMutPass i) = "immutable variable " ++ show i ++ " was passed to a mutable reference argument."
