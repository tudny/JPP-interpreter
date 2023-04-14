module Src.TypeChecker where

import Src.Jabba.Abs ( Ident,
                       Program' (..), Program (..),
                       Instr' (..), Instr (..),
                       Arg' (..), Arg (..),
                       Item' (..), Item (..),
                       Decl' (..), Decl (..),
                       Block (..), Type (..),
                       PlsOp (..), MulOp (..),
                       NotOp (..), NegOp (..),
                       Expr' (..), Expr (..),
                       BNFC'Position, Ident (..)
                       )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (StateT (runStateT), State, runState, evalState, MonadState (get, put), modify)
import Data.List ( nub )
import qualified Data.Maybe
import Src.Errors
    ( ErrType (..),
      ErrHolder (TypeChecker),
      Err )
import Src.Types ( VarMutability(..), VarType(..), absTypeToVarType, VarRef (VRRef, VRCopy) )
import Debug.Trace (trace)

type RetType = Maybe VarType

type TypeEnv = Map.Map Ident (VarType, VarMutability)
type IM a = ExceptT ErrHolder (State TypeEnv) a


localState :: (TypeEnv -> TypeEnv) -> IM a -> IM a
localState f m = do
    s <- get
    put (f s)
    r <- m
    put s
    return r


typeCheck :: Program -> Err ()
typeCheck = typeCheckWithEnv Map.empty


typeCheckWithEnv :: TypeEnv -> Program -> Either ErrHolder ()
typeCheckWithEnv env p = evalState (runExceptT (checkTypeP p)) env


checkTypeP :: Program -> IM ()
checkTypeP (PProgram _ is) = mapM_ checkTypeI is


checkTypeI :: Instr -> IM RetType
checkTypeI (IUnit _) = pure Nothing
checkTypeI (IExpr _ e) = checkTypeE e >> pure Nothing
checkTypeI (IIncr pos v) = opOnVarType pos v [VTInt]
checkTypeI (IDecr pos v) = opOnVarType pos v [VTInt]
checkTypeI (IDecl _ d) = checkTypeD d
checkTypeI i = pure Nothing -- TODO: implement


checkTypeD :: Decl -> IM RetType
checkTypeD (DVar pos its) = declareNewVariables pos its VMMut
checkTypeD (DVal pos its) = declareNewVariables pos its VMConst


checkTypeItem :: Item -> IM (Ident, VarType, BNFC'Position)
checkTypeItem (DItemVal pos v t e) = do
    let t' = absTypeToVarType t
    et' <- checkTypeE e
    if t' == et'
        then pure (v, t', pos)
        else throwError $ TypeChecker pos $ WrongType v t' [et']
checkTypeItem (DItem pos v t) = pure (v, absTypeToVarType t, pos)


checkTypeE :: Expr -> IM VarType
checkTypeE (EVarName pos v) = fst <$> getVarType pos v
checkTypeE (EIntLit _ _) = pure VTInt
checkTypeE (EStringLit _ _) = pure VTString
checkTypeE (EBoolLitFalse _) = pure VTBool
checkTypeE (EBoolLitTrue _) = pure VTBool
checkTypeE (ERun pos fn es) = do
    env <- get
    (args, ret) <- getF pos fn
    callArgs <- mapM (getCallExprType pos) es
    if length args /= length callArgs
        then throwError $ TypeChecker pos $ WrongNumberOfArgs fn (length args) (length callArgs)
        else do
            let args' = zip3 callArgs args [0..]
            mapM_ (checkCallArgument pos) args'
            pure ret
checkTypeE _ = pure VTVoid -- TODO: implement


getVarType :: BNFC'Position -> Ident -> IM (VarType, VarMutability)
getVarType pos v = do
    env <- get
    case Map.lookup v env of
        Just (t, m) -> pure (t, m)
        Nothing -> throwError $ TypeChecker pos $ NotDeclVar v


getF :: BNFC'Position -> Ident -> IM ([(VarType, VarMutability, VarRef)], VarType)
getF pos fn = do
    env <- get
    case Map.lookup fn env of
        Just (VTFun args ret, _) -> pure (args, ret)
        _ -> throwError $ TypeChecker pos $ NotDeclFun fn


getCallExprType :: BNFC'Position -> Expr -> IM (VarType, Maybe VarMutability)
getCallExprType _ (EVarName pos v) = do
    (t, m) <- getVarType undefined v
    pure (t, Just m)
getCallExprType _ e = do
    t <- checkTypeE e
    pure (t, Nothing)


checkCallArgument :: BNFC'Position -> ((VarType, Maybe VarMutability), (VarType, VarMutability, VarRef), Int) -> IM ()
checkCallArgument pos ((t, mm), (t', m, r), id) = do
    checkCallArgumentType pos t t' id
    checkCallArgumentMutability pos mm (m, r) id


checkCallArgumentType :: BNFC'Position -> VarType -> VarType -> Int -> IM ()
checkCallArgumentType pos t t' id = do
    if t == t'
        then pure ()
        else throwError $ TypeChecker pos $ WrongTypeArg id t t'


-- There are 12 cases to check, but only 2 are problematic.
checkCallArgumentMutability :: BNFC'Position -> Maybe VarMutability -> (VarMutability, VarRef) -> Int -> IM ()
checkCallArgumentMutability pos Nothing (_, VRRef) id = throwError $ TypeChecker pos $ ExprMutPass id
checkCallArgumentMutability pos (Just VMConst) (VMMut, VRRef) id = throwError $ TypeChecker pos $ ImmutMutPass id
checkCallArgumentMutability _ _ _ _ = pure ()


opOnVarType :: BNFC'Position -> Ident -> [VarType] -> IM RetType
opOnVarType pos v ex = do
    (t, m) <- getVarType pos v
    case t of
        VTInt -> case m of
            VMMut -> pure Nothing
            VMConst -> throwError $ TypeChecker pos $ ImmutVar v
        _ -> throwError $ TypeChecker pos $ WrongType v t [VTInt]


checkAllNamesAreUnique :: Set.Set Ident -> [(Ident, VarType, BNFC'Position)] -> IM ()
checkAllNamesAreUnique _ [] = pure ()
checkAllNamesAreUnique s ((v, t, pos):xs) = do
    if Set.member v s
        then throwError $ TypeChecker pos $ VarAlreadyDecl v
        else checkAllNamesAreUnique (Set.insert v s) xs


declareNewVariables :: BNFC'Position -> [Item' BNFC'Position] -> VarMutability -> IM RetType
declareNewVariables pos its m = do
    items <- mapM checkTypeItem its
    checkAllNamesAreUnique Set.empty items
    mapM_ (\(v, t, _) -> modify (Map.insert v (t, m))) items
    pure Nothing

