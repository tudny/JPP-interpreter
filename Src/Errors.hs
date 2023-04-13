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
