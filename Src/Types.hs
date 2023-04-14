module Src.Types where

import Src.Jabba.Abs ( Type' (..), Type (..) )

data VarType 
    = VTInt 
    | VTBool 
    | VTString 
    | VTVoid 
    deriving (Eq)

type FnArg = (VarType, VarMutability, VarRef)
data FnType = Fn [FnArg] VarType deriving (Eq)


data VarMutability
    = VMMut 
    | VMConst 
    deriving (Eq)


data VarRef
    = VRRef
    | VRCopy
    deriving (Eq)


absTypeToVarType :: Type -> VarType
absTypeToVarType (TInt _) = VTInt
absTypeToVarType (TBool _) = VTBool
absTypeToVarType (TString _) = VTString
absTypeToVarType (TVoid _) = VTVoid
absTypeToVarType TGen {} = undefined


instance Show VarType where
    show VTInt = "Integer"
    show VTString = "String"
    show VTBool = "Boolean"
    show VTVoid = "Unit"


instance Show FnType where
    show (Fn args ret) = "(" ++ show args ++ ") -> " ++ show ret


instance Show VarMutability where
    show VMMut = "mut"
    show VMConst = "const"


instance Show VarRef where
    show VRRef = "ref"
    show VRCopy = "copy"
