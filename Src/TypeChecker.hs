module Src.TypeChecker where

import Src.Jabba.Abs ( Ident,
                       Program' (..), Program (..),
                       Instr' (..), Instr (..),
                       Arg' (..), Arg (..),
                       Item (..), Decl (..),
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
import Src.Types ( VarMutability(..), VarType(..) )

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
typeCheck p = evalState (runExceptT (checkTypeP p)) Map.empty

checkTypeP :: Program -> IM ()
checkTypeP (PProgram _ is) = mapM_ checkTypeI is 

checkTypeI :: Instr -> IM RetType
checkTypeI (IUnit _) = pure Nothing
checkTypeI (IIncr pos v) = do
    (t, m) <- getVarType pos v
    case t of
        VTInt -> case m of
            VMMut -> pure Nothing
            VMConst -> throwError $ TypeChecker pos $ ImmutVar v
        _ -> throwError $ TypeChecker pos $ WrongType v t [VTInt]
checkTypeI i = pure Nothing

getVarType :: BNFC'Position -> Ident -> IM (VarType, VarMutability)
getVarType pos v = do
    env <- get
    case Map.lookup v env of
        Just (t, m) -> pure (t, m)
        Nothing -> throwError $ TypeChecker pos $ NotDeclVar v
