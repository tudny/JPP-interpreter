module Src.TypeChecker where

import Src.Jabba.Abs ( Ident, Program (..),
                       Instr (..), Arg (..),
                       Item (..), Decl (..),
                       Block (..), Type (..),
                       PlsOp (..), MulOp (..),
                       NotOp (..), NegOp (..),
                       Expr (..)
                       )
import qualified Data.Map as Map
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (StateT (runStateT), State, runState, evalState, MonadState (get, put), modify)
import Data.List ( nub )
import qualified Data.Maybe

type RetType = Maybe VarType
data VarType = VTInt | VTBool | VTString | VTVoid | VTFun [VarType] VarType deriving (Eq)
data VarMutability = VMMut | VMConst deriving (Eq)

type Err = Either String
type TypeEnv = Map.Map Ident (VarType, VarMutability)
type IM a = ExceptT String (State TypeEnv) a

localState :: (TypeEnv -> TypeEnv) -> IM a -> IM a
localState f m = do
    s <- get
    put (f s)
    r <- m
    put s
    return r

typeCheck :: Program -> Err ()
typeCheck p = evalState (runExceptT (checkTypeP p)) Map.empty

checkTypeP :: Program -> IM ()
checkTypeP (PProgram is) = mapM_ checkTypeI is

checkTypeI :: Instr -> IM RetType
checkTypeI IUnit = pure Nothing
checkTypeI (IDecl d) = checkTypeD d >> pure Nothing
checkTypeI (IExpr e) = checkTypeE e >> pure Nothing
checkTypeI (DFun i as t b) = do
    vars <- mapM getArgType as
    -- check if all variables are unique
    let varNames = map (\(i, _, _) -> i) vars
    if length varNames /= length (nub varNames)
        then throwError "Duplicate variable names in function arguments"
        else pure ()
    -- add variables to type environment
    let varTypes = map (\(i, t, m) -> (i, (t, m))) vars
    retType <- localState (Map.union (Map.fromList varTypes)) (checkTypeB b)
    let resolvedReturnType = Data.Maybe.fromMaybe VTVoid retType
    if resolvedReturnType /= mapArgType t
        then throwError "Return type does not match function type"
        else 
            -- add function to type environment
            let funType = VTFun (map (\(_, t, _) -> t) vars) (mapArgType t)
            in localState (Map.insert i (funType, VMConst)) (pure Nothing)
checkTypeI (IIncr v) = do
    integerMutableOperation v
    pure Nothing
checkTypeI (IDecr v) = do
    integerMutableOperation v
    pure Nothing
checkTypeI (IAss v e) = do
    (vType, vMut) <- getVarType v
    eType <- checkTypeE e
    if vType /= eType
        then throwError "Assignment type mismatch"
        else if vMut /= VMMut
            then throwError "Assignment to immutable variable"
            else pure Nothing
checkTypeI (IRet e) = do
    eType <- checkTypeE e
    pure $ Just eType
checkTypeI IRetUnit = pure $ Just VTVoid
checkTypeI (IIfElse e b1 b2) = do
    eType <- checkTypeE e
    if eType /= VTBool
        then throwError "If condition must be a boolean"
        else do
            b1Type <- checkTypeB b1
            b2Type <- checkTypeB b2
            if b1Type == b2Type
                then pure b1Type
                else throwError "If branches have different return types"
checkTypeI (IIf e b) = do 
    eType <- checkTypeE e
    if eType /= VTBool
        then throwError "If condition must be a boolean"
        else checkTypeB b
checkTypeI (IWhile e b) = do
    eType <- checkTypeE e
    if eType /= VTBool
        then throwError "While condition must be a boolean"
        else do 
            blockRetType <- checkTypeB b
            case blockRetType of
                Nothing -> pure Nothing
                Just _  -> throwError "While block cannot return a value"
checkTypeI (IWhileFin e b1 b2) = do
    eType <- checkTypeE e
    if eType /= VTBool
        then throwError "While condition must be a boolean"
        else do
            b1Type <- checkTypeB b1
            b2Type <- checkTypeB b2
            if b1Type == b2Type
                then pure b1Type
                else throwError "While branches have different return types"
checkTypeI (IFor i e1 e2 b) = do
    e1Type <- checkTypeE e1
    e2Type <- checkTypeE e2
    if e1Type /= VTInt || e2Type /= VTInt
        then throwError "For loop bounds must be integers"
        else do
            blockRetType <- localState (Map.insert i (VTInt, VMConst)) (checkTypeB b)
            case blockRetType of
                Nothing -> pure Nothing
                Just _  -> throwError "For block cannot return a value"
checkTypeI IYieldUnit = throwError "Yield not supported"
checkTypeI IYield {} = throwError "Yield e not supported"
checkTypeI IForGen {} = throwError "Yield for not supported"


integerMutableOperation :: Ident -> IM ()
integerMutableOperation v = do
    (vType, vMut) <- getVarType v
    if vType /= VTInt
        then throwError "Integer operation can only be used on integers"
        else if vMut /= VMMut
            then throwError "Integer operation can only be used on mutable variables"
            else pure ()

getVarType :: Ident -> IM (VarType, VarMutability)
getVarType i = do
    env <- get
    case Map.lookup i env of
        Just (t, m) -> pure (t, m)
        Nothing -> throwError $ "Variable " ++ show i ++ " not found"

mapArgType :: Type -> VarType
mapArgType TInt = VTInt
mapArgType TBool = VTBool
mapArgType TString = VTString
mapArgType TVoid = VTVoid
mapArgType (TGen t) = undefined

getArgType :: Arg -> IM (Ident, VarType, VarMutability)
getArgType (RefMutArg i t) = pure (i, mapArgType t, VMMut)
getArgType (RefConstArg i t) = pure (i, mapArgType t, VMConst)
getArgType (CopyMutArg i t) = pure (i, mapArgType t, VMMut)
getArgType (CopyConstArg i t) = pure (i, mapArgType t, VMConst)

checkTypeItem :: Item -> IM VarType
checkTypeItem (DItemVal i t e) = do
    eType <- checkTypeE e
    if eType /= mapArgType t
        then throwError "Item value type mismatch"
        else pure $ mapArgType t
checkTypeItem (DItem i t) = do 
    pure $ mapArgType t

checkTypeVariableDeclaration :: [Item] -> VarMutability -> IM RetType
checkTypeVariableDeclaration is mut = do
    -- check if names are unique
    let names = map (\(DItem i _) -> i) is
    if length names /= length (nub names)
        then throwError "Duplicate variable names in declaration"
        else pure ()
    types <- mapM checkTypeItem is
    let varaibles = [(i, (t, mut)) | (i, t) <- zip names types]
    modify (Map.union (Map.fromList varaibles))
    pure Nothing

checkTypeD :: Decl -> IM RetType
checkTypeD (DVar is) = checkTypeVariableDeclaration is VMMut
checkTypeD (DVal is) = checkTypeVariableDeclaration is VMConst

checkTypeE :: Expr -> IM VarType
checkTypeE (EVarName i) = do
    (t, _) <- getVarType i
    pure t
checkTypeE (EIntLit _) = pure VTInt
checkTypeE EBoolLitTrue = pure VTBool
checkTypeE EBoolLitFalse = pure VTBool
checkTypeE (ERun i es) = do
    (t, _) <- getVarType i
    case t of
        VTFun argTypes retType -> do
            if length argTypes /= length es
                then throwError "Function call argument count mismatch"
                else do
                    esTypes <- mapM checkTypeE es
                    if argTypes /= esTypes
                        then throwError "Function call argument type mismatch"
                        else pure retType
        _ -> throwError "Function call on non-function"
checkTypeE (EStringLit _) = pure VTString
checkTypeE (ENeg op e) = do
    eType <- checkTypeE e
    if eType /= VTInt
        then throwError "Negation can only be used on integers"
        else pure VTInt
checkTypeE (ENot op e) = do
    eType <- checkTypeE e
    if eType /= VTBool
        then throwError "Negation can only be used on booleans"
        else pure VTBool
checkTypeE (EMul e1 op e2) = do
    e1Type <- checkTypeE e1
    e2Type <- checkTypeE e2
    if e1Type /= VTInt || e2Type /= VTInt
        then throwError "Multiplication can only be used on integers"
        else pure VTInt
checkTypeE (ESum e1 op e2) = do
    e1Type <- checkTypeE e1
    e2Type <- checkTypeE e2
    if e1Type /= VTInt || e2Type /= VTInt
        then throwError "Sum can only be used on integers"
        else pure VTInt
checkTypeE (ERel e1 op e2) = do
    e1Type <- checkTypeE e1
    e2Type <- checkTypeE e2
    if e1Type /= VTInt || e2Type /= VTInt
        then throwError "Relational operation can only be used on integers"
        else pure VTBool
checkTypeE (EBAnd e1 op e2) = do
    e1Type <- checkTypeE e1
    e2Type <- checkTypeE e2
    if e1Type /= VTBool || e2Type /= VTBool
        then throwError "Bitwise and can only be used on booleans"
        else pure VTBool
checkTypeE (EBOr e1 op e2) = do
    e1Type <- checkTypeE e1
    e2Type <- checkTypeE e2
    if e1Type /= VTBool || e2Type /= VTBool
        then throwError "Bitwise or can only be used on booleans"
        else pure VTBool
checkTypeE (ETer b e1 e2) = do
    bType <- checkTypeE b
    e1Type <- checkTypeE e1
    e2Type <- checkTypeE e2
    if bType /= VTBool
        then throwError "Ternary operator condition must be a boolean"
        else if e1Type /= e2Type
            then throwError "Ternary operator branches must have the same type"
            else pure e1Type

checkTypeB :: Block -> IM RetType
checkTypeB (IBlock is) = do
    retTypes <- mapM checkTypeI is
    let retTypes' = filter (/= Nothing) retTypes
    -- check if there was a return statement
    case retTypes' of
        []     -> pure Nothing
        (x:xs) -> if all (== x) xs
            then pure x
            else throwError "Return statements have different types"
