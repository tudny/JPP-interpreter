module Src.Evaluator where

import Src.Jabba.Abs ( Ident,
                       Program' (..), Program (..),
                       Instr' (..), Instr (..),
                       Arg' (..), Arg (..),
                       Item' (..), Item (..),
                       Decl' (..), Decl (..),
                       Block' (..), Block (..),
                       Type' (..), Type (..),
                       PlsOp' (..), PlsOp (..),
                       MulOp' (..), MulOp (..),
                       NotOp' (..), NotOp (..),
                       NegOp' (..), NegOp (..),
                       RelOp' (..), RelOp (..),
                       AndOp' (..), AndOp (..),
                       OrOp' (..), OrOp (..),
                       Expr' (..), Expr (..),
                       Ident (..), HasPosition (hasPosition),
                       BNFC'Position, TArg' (..), TArg (..)
                       )

import Src.Types ( VarRef (..) )
import Src.Errors ( ErrHolder, Err (..), ErrHolder (RuntimeError), RuntimeType (..) )
import Control.Monad.Except (ExceptT, runExceptT, throwError, void)
import Control.Monad.Reader (ReaderT, runReaderT, ask, local)
import Control.Monad.State (StateT, evalStateT, get, put, modify)
import qualified Data.Map as Map (Map, empty, insert, lookup, fromList)
import Debug.Trace (trace) -- TODO: remove

-- =============================================================================

newtype Env = Env { env :: Map.Map Ident Loc }

emptyEnv :: Env
emptyEnv = Env Map.empty

envGet :: Ident -> Env -> IM Loc
envGet i (Env m) = case Map.lookup i m of
    Nothing -> throwError $ RuntimeError Nothing TypeCheckerDidntCatch
    Just l -> pure l

type ModEnv = Env -> Env

-- =============================================================================

type FnArg = (Ident, VarRef)
data Value
    = VTInt Integer
    | VTBool Bool
    | VTString String
    | VTUnit
    | VTFun [FnArg] Block Env

data LoopControlFlow = Break | Continue

type RetType = (Maybe Value, Maybe LoopControlFlow)

type Loc = Int

data Store = Store { nextFree :: Loc, memory :: Map.Map Loc Value }

emptyStore :: Store
emptyStore = Store 0 Map.empty

newlock :: Store -> (Loc, Store)
newlock (Store n m) = (n, Store (n + 1) m)

insert :: Loc -> Value -> Store -> Store
insert l v (Store n m) = Store n (Map.insert l v m)

storeGet :: Loc -> Store -> IM Value
storeGet l (Store _ m) = case Map.lookup l m of
    Nothing -> throwError $ RuntimeError Nothing TypeCheckerDidntCatch
    Just v -> pure v

type ModStore = Store -> Store

-- =============================================================================

--                  errors     local   env  modify memory io
type IM a = ExceptT ErrHolder (ReaderT Env (StateT Store IO)) a


evaluate :: Program -> IO (Err ())
evaluate p = do 
    let errorsT = runExceptT (evalP p)
    let envT = runReaderT errorsT emptyEnv
    evalStateT envT emptyStore



evalP :: Program -> IM ()
evalP (PProgram _ is) = void $ evalIs is



evalIs :: [Instr] -> IM RetType

evalIs [] = pure (Nothing, Nothing)

evalIs ((IExpr _ e):is) = evalE e >> evalIs is
evalIs ((IDecl _ d):is) = do
    mod <- evalD d
    local mod $ evalIs is

evalIs _ = undefined -- TODO: implement



evalD :: Decl -> IM ModEnv
evalD (DVar _ vars) = addVarsToEnv vars
evalD (DVal _ vars) = addVarsToEnv vars



addVarsToEnv :: [Item] -> IM ModEnv
addVarsToEnv vars = do
    items <- mapM resolveItem vars
    store <- get
    -- first we reserve memory for all variables
    let (locs, store') = foldl (\ (ls, s) _ -> let (nl, s') = newlock s in (nl:ls, s')) ([], store) items
    put store'
    -- then we insert values into memory
    mapM_ (\ ((i, v), l) -> modify $ insert l v) $ zip items locs
    let ids = map fst items
    -- we need to tell how to modify environment
    let envMod (Env m) = Env $ foldl (\ m' (i, l) -> Map.insert i l m') m $ zip ids locs
    pure envMod



resolveItem :: Item -> IM (Ident, Value)
resolveItem (DItemVal _ i _ e) = do
    v <- evalE e
    pure (i, v)
resolveItem (DItemAuto _ i e) = do
    v <- evalE e
    pure (i, v)
resolveItem (DItem _ i t) = pure (i, defaultValueForType t)



defaultValueForType :: Type -> Value
defaultValueForType (TInt _) = VTInt 0
defaultValueForType (TBool _) = VTBool False
defaultValueForType (TString _) = VTString ""
defaultValueForType (TVoid _) = VTUnit
defaultValueForType TFun {} = undefined -- type checker should catch this



evalB :: Block -> IM RetType
evalB _ = undefined -- TODO: implement



evalE :: Expr -> IM Value

evalE (EVarName pos v) = getVarValue pos v
evalE (EIntLit _ n) = pure $ VTInt n
evalE (EBoolLitTrue _) = pure $ VTBool True
evalE (EBoolLitFalse _) = pure $ VTBool False
evalE (EStringLit _ s) = pure $ VTString s
evalE (ENeg _ (ONeg _) e) = do
    (VTInt n) <- evalE e
    pure (VTInt $ negate n)
evalE (ENot _ (ONot _) e) = do
    (VTBool b) <- evalE e
    pure (VTBool $ not b)
evalE (EMul _ e1 op e2) = do
    (VTInt n1) <- evalE e1
    (VTInt n2) <- evalE e2
    case op of
        OMul _ -> pure (VTInt $ n1 * n2)
        ODiv _ -> pure (VTInt $ n1 `div` n2)
        OMod _ -> pure (VTInt $ n1 `mod` n2)
evalE (ESum pos e1 op e2) = do
    v1 <- evalE e1
    v2 <- evalE e2
    case (op, v1, v2) of
        (OPlus  _, VTInt n1, VTInt n2) -> pure (VTInt $ n1 + n2)
        (OMinus _, VTInt n1, VTInt n2) -> pure (VTInt $ n1 - n2)
        (OPlus  _, VTString s1, VTString s2) -> pure (VTString $ s1 ++ s2)
        _ -> throwError $ RuntimeError pos TypeCheckerDidntCatch
evalE (ERel _ e1 op e2) = do
    v1 <- evalE e1
    v2 <- evalE e2
    b <- doRelOp op v1 v2
    pure $ VTBool b
evalE (EBAnd _ e1 (OAnd _) e2) = do
    (VTBool b1) <- evalE e1
    if b1
        then do
            (VTBool b2) <- evalE e2
            pure $ VTBool b2
        else pure $ VTBool False
evalE (EBOr _ e1 (OOr _) e2) = do
    (VTBool b1) <- evalE e1
    if b1
        then pure $ VTBool True
        else do
            (VTBool b2) <- evalE e2
            pure $ VTBool b2
evalE (ETer _ eb e1 e2) = do
    (VTBool b) <- evalE eb
    if b
        then evalE e1
        else evalE e2


evalE _ = undefined -- TODO: implement


doRelOp :: RelOp -> Value -> Value -> IM Bool
doRelOp (REq _)  (VTInt n1)    (VTInt n2)    = pure $ n1 == n2
doRelOp (RNeq _) (VTInt n1)    (VTInt n2)    = pure $ n1 /= n2
doRelOp (RLt _)  (VTInt n1)    (VTInt n2)    = pure $ n1 <  n2
doRelOp (RGt _)  (VTInt n1)    (VTInt n2)    = pure $ n1 >  n2
doRelOp (RLeq _) (VTInt n1)    (VTInt n2)    = pure $ n1 <= n2
doRelOp (RGeq _) (VTInt n1)    (VTInt n2)    = pure $ n1 >= n2
doRelOp (REq _)  (VTBool b1)   (VTBool b2)   = pure $ b1 == b2
doRelOp (RNeq _) (VTBool b1)   (VTBool b2)   = pure $ b1 /= b2
doRelOp (REq _)  (VTString s1) (VTString s2) = pure $ s1 == s2
doRelOp (RNeq _) (VTString s1) (VTString s2) = pure $ s1 /= s2
doRelOp _ _ _ = throwError $ RuntimeError Nothing TypeCheckerDidntCatch



-- =============================================================================



getVarLoc :: BNFC'Position -> Ident -> IM Loc
getVarLoc pos v = do
    env <- ask
    envGet v env


getVarValue :: BNFC'Position -> Ident -> IM Value
getVarValue pos v = do
    loc <- getVarLoc pos v
    store <- get
    storeGet loc store


