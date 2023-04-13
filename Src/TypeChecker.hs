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
                       Expr (..), BNFC'Position,
                       Ident (..)
                       )
import qualified Data.Map as Map
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (StateT (runStateT), State, runState, evalState, MonadState (get, put), modify)
import Data.List ( nub )
import qualified Data.Maybe
import Src.Errors
    ( ErrType (..),
      ErrHolder (TypeChecker),
      Err )
import Src.Types ( VarMutability(..), VarType(..), absTypeToVarType )

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
checkTypeI (IIncr pos v) = opOnVarType pos v [VTInt]
checkTypeI (IDecr pos v) = opOnVarType pos v [VTInt]
checkTypeI (IDecl _ d) = checkTypeD d
checkTypeI i = pure Nothing

checkTypeD :: Decl -> IM RetType
checkTypeD (DVar pos its) = mapM_ (checkTypeItem) its >> pure Nothing
checkTypeD d = pure Nothing

checkTypeItem :: Item -> IM (Ident, VarType)
checkTypeItem (DItemVal pos v t e) = do
    let t' = absTypeToVarType t
    et' <- checkTypeE e
    if t' == et' 
        then pure (v, t')
        else throwError $ TypeChecker pos $ WrongType v t' [et'] 
checkTypeItem _ = pure (Ident "", VTVoid)


checkTypeE :: Expr -> IM VarType
checkTypeE _ = pure VTVoid

getVarType :: BNFC'Position -> Ident -> IM (VarType, VarMutability)
getVarType pos v = do
    env <- get
    case Map.lookup v env of
        Just (t, m) -> pure (t, m)
        Nothing -> throwError $ TypeChecker pos $ NotDeclVar v

opOnVarType :: BNFC'Position -> Ident -> [VarType] -> IM RetType
opOnVarType pos v ex = do
    (t, m) <- getVarType pos v
    case t of
        VTInt -> case m of
            VMMut -> pure Nothing
            VMConst -> throwError $ TypeChecker pos $ ImmutVar v
        _ -> throwError $ TypeChecker pos $ WrongType v t [VTInt]
