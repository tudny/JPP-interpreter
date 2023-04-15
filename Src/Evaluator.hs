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
import Control.Monad.State (StateT, evalStateT, get, put, modify, gets)
import qualified Data.Map as Map (Map, empty, insert, lookup, fromList)
import Debug.Trace (trace) -- TODO: remove
-- TODO: remove
import Data.Maybe (isJust, isNothing)
import Data.List (zip4)

-- =============================================================================

newtype Env = Env { env :: Map.Map Ident Loc }

emptyEnv :: Env
emptyEnv = Env Map.empty

envGet :: Ident -> Env -> IM Loc
envGet i (Env m) = case Map.lookup i m of
    Nothing -> throwError $ RuntimeError Nothing TypeCheckerDidntCatch
    Just l -> pure l

insertEnv :: Ident -> Loc -> Env -> Env
insertEnv i l (Env m) = Env $ Map.insert i l m

insertEnvMany :: [(Ident, Loc)] -> Env -> Env
insertEnvMany [] e = e
insertEnvMany ((i, l):xs) e = insertEnvMany xs $ insertEnv i l e

type ModEnv = Env -> Env

-- =============================================================================

type FnArg = (Ident, VarRef)
data Value
    = VTInt Integer
    | VTBool Bool
    | VTString String
    | VTUnit
    | VTFun [FnArg] Block Env

instance Show Value where
    show (VTInt i) = show i
    show (VTBool b) = show b
    show (VTString s) = show s
    show VTUnit = "()"
    show VTFun {} = "<function>"

data LoopControlFlow = Break | Continue

type RetType = (Maybe Value, Maybe LoopControlFlow)

type Loc = Int

data Store = Store { nextFree :: Loc, memory :: Map.Map Loc Value }

emptyStore :: Store
emptyStore = Store 0 Map.empty

newlock :: Store -> (Loc, Store)
newlock (Store n m) = (n, Store (n + 1) m)

newlockM :: IM Loc
newlockM = do
    s <- get
    let (l, s') = newlock s
    put s'
    pure l

insert :: Loc -> Value -> Store -> Store
insert l v (Store n m) = Store n (Map.insert l v m)

storeGet :: Loc -> Store -> IM Value
storeGet l (Store _ m) = case Map.lookup l m of
    Nothing -> throwError $ RuntimeError Nothing TypeCheckerDidntCatch
    Just v -> pure v

reserveN :: Int -> IM [Loc]
reserveN n = do
    store <- get
    -- first we reserve memory for all variables
    let (locs, store') = foldl (\ (ls, s) _ -> let (nl, s') = newlock s in (nl:ls, s')) ([], store) [1..n]
    put store'
    pure locs

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
evalIs ((IExpr _ e):is) = do
    n <- evalE e
    trace ("Expr: " ++ show n) $ evalIs is
evalIs ((IDecl _ d):is) = do
    mod <- evalD d
    local mod $ evalIs is
evalIs ((IUnit _):is) = evalIs is
evalIs ((IIncr _ v):is) = modIntVar v (+1) >> evalIs is
evalIs ((IDecr _ v):is) = modIntVar v (subtract 1) >> evalIs is
evalIs ((IAss _ v e):is) = do 
    n <- evalE e
    loc <- envGet v =<< ask
    modify $ insert loc n
    evalIs is
evalIs ((IRet _ e):_) = do
    v <- evalE e
    pure (Just v, Nothing)
evalIs ((IRetUnit _):_) = pure (Just VTUnit, Nothing)
evalIs ((IBreak _):_) = pure (Nothing, Just Break)
evalIs ((ICont _):_) = pure (Nothing, Just Continue)
evalIs ((IIf pos eb b):is) = evalIs (IIfElse pos eb b (IBlock pos []):is)
evalIs ((IIfElse _ eb b1 b2):is) = do
    (VTBool b) <- evalE eb
    if b then evalB b1 >>= handleRetType is
         else evalB b2 >>= handleRetType is
evalIs all@((IWhile pos e b):is) = do
    (VTBool cond) <- evalE e
    if cond then do
        (ret, flow) <- evalB b
        if isJust ret then pure (ret, Nothing)
        else case flow of
            Nothing -> evalIs all
            Just Break -> pure (Nothing, Nothing)
            Just Continue -> evalIs all
    else evalIs is
evalIs all@((IWhileFin pos e b bFin):is) = do
    (VTBool cond) <- evalE e
    if cond then do
        (ret, flow) <- evalB b
        if isJust ret then pure (ret, Nothing)
        else case flow of
            Nothing -> evalIs all
            Just Break -> pure (Nothing, Nothing)
            Just Continue -> evalIs all
    else evalB bFin >> evalIs is
evalIs ((IFor _ v e1 e2 b):is) = do
    (VTInt n1) <- evalE e1
    (VTInt n2) <- evalE e2
    let range = [n1..n2]
    loc <- newlockM
    local (insertEnv v loc) (runFor loc range b) >>= handleRetType is
evalIs ((IBBlock _ b):is) = evalB b >>= handleRetType is
evalIs ((DFun _ fN args t b):is) = do
    let args' = map resolveDeclArg args
    env <- ask
    loc <- newlockM
    let f = VTFun args' b (insertEnv fN loc env)
    modify $ insert loc f
    local (insertEnv fN loc) (evalIs is)
evalIs ((DFunUnit pos fN args b):is) = evalIs (DFun pos fN args (TVoid pos) b:is)



resolveDeclArg :: Arg -> (Ident, VarRef)
resolveDeclArg (RefMutArg    _ i _) = (i, VRRef)
resolveDeclArg (RefConstArg  _ i _) = (i, VRRef)
resolveDeclArg (CopyMutArg   _ i _) = (i, VRCopy)
resolveDeclArg (CopyConstArg _ i _) = (i, VRCopy)



runFor :: Loc -> [Integer] -> Block -> IM RetType
runFor loc [] _ = pure (Nothing, Nothing)
runFor loc (n:ns) b = do
    modify $ insert loc (VTInt n)
    (ret, flow) <- evalB b
    if isJust ret then pure (ret, Nothing)
    else case flow of
        Nothing -> runFor loc ns b
        Just Break -> pure (Nothing, Nothing)
        Just Continue -> runFor loc ns b



handleRetType :: [Instr] -> RetType -> IM RetType
handleRetType is (Nothing, Nothing) = evalIs is
handleRetType is (Just _, Just _) = undefined -- There should never be a return and a break/continue
handleRetType is (ret, Nothing) = pure (ret, Nothing)
handleRetType is (Nothing, controlFlow) = pure (Nothing, controlFlow)



modIntVar :: Ident -> (Integer -> Integer) -> IM ()
modIntVar v f = do
    loc <- envGet v =<< ask
    store <- get
    (VTInt n) <- storeGet loc store
    modify $ insert loc (VTInt $ f n)



evalD :: Decl -> IM ModEnv
evalD (DVar _ vars) = addItemsToEnv vars
evalD (DVal _ vars) = addItemsToEnv vars



addItemsToEnv :: [Item] -> IM ModEnv
addItemsToEnv vars = do
    items <- mapM resolveItem vars
    addVarsToEnv items



addVarsToEnv :: [(Ident, Value)] -> IM ModEnv
addVarsToEnv items = do
    locs <- reserveN $ length items
    -- then we insert values into memory
    mapM_ (\ ((_, v), l) -> modify $ insert l v) $ zip items locs
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
evalB (IBlock _ is) = evalIs is



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
evalE (ERun pos e argsE) = do
    (VTFun args b env) <- evalE e
    argsVMl <- mapM evalERef argsE
    let (argsV, argsMl) = unzip argsVMl
    let (argsI, argsR) = unzip args
    argsL <- mapM insertArg $ zip3 argsR argsV argsMl
    let envInserter = insertEnvMany $ zip argsI argsL
    (ret, Nothing) <- local envInserter $ evalB b
    case ret of 
        Nothing -> pure VTUnit
        Just v  -> pure v
    where
        insertArg :: (VarRef, Value, Maybe Loc) -> IM Loc
        insertArg (r, v, ml) = do
            loc <- case r of
              VRRef -> do
                let Just l = ml
                pure l
              VRCopy -> do
                [l] <- reserveN 1
                pure l
            modify $ insert loc v
            pure loc



evalE _ = undefined -- TODO: implement




evalERef :: Expr -> IM (Value, Maybe Loc)
evalERef (EVarName pos v) = do
    loc <- envGet v =<< ask
    store <- get
    v <- storeGet loc store
    pure (v, Just loc)
evalERef e = do
    v <- evalE e
    pure (v, Nothing)



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


