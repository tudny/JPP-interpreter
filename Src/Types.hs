module Src.Types where

data VarType 
    = VTInt 
    | VTBool 
    | VTString 
    | VTVoid 
    | VTFun [VarType] VarType 
    deriving (Eq)


data VarMutability 
    = VMMut 
    | VMConst 
    deriving (Eq)


instance Show VarType where
    show VTInt = "Integer"
    show VTString = "String"
    show VTBool = "Boolean"
    show VTVoid = "Unit"
    show (VTFun args ret) = "(" ++ show args ++ ") -> " ++ show ret


instance Show VarMutability where
    show VMMut = "mut"
    show VMConst = "const"
