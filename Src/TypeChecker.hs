module Src.TypeChecker where

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
                       BNFC'Position, Ident (..), HasPosition (hasPosition)
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
import Src.Types ( VarMutability (..), VarType (..), absTypeToVarType, VarRef (..) )
import Debug.Trace (trace)

data RetType'
    = None
    | Definitive VarType
    | Branching VarType

--             return     loop flow control
type RetType = (RetType', Bool)

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
checkTypeP (PProgram pos is) = do
    retType <- checkTypeB (IBlock pos is)
    case retType of
        (None, False) -> pure ()
        (Definitive t, _) -> throwError $ TypeChecker pos $ TopLevelProgramReturn t
        (Branching t, _) -> throwError $ TypeChecker pos $ TopLevelProgramMaybeReturn t
        (None, True) -> throwError $ TypeChecker pos TopLevelProgramLoopFlow


checkTypeI :: Instr -> IM RetType
checkTypeI (IUnit _) = pure (None, False)
checkTypeI (IExpr _ e) = checkTypeE e >> pure (None, False)
checkTypeI (IIncr pos v) = opOnVarType pos v [VTInt]
checkTypeI (IDecr pos v) = opOnVarType pos v [VTInt]
checkTypeI (IDecl _ d) = checkTypeD d
checkTypeI (IAss pos v e) = do
    (t, m) <- getVarType pos v
    et <- checkTypeE e
    if m == VMConst
        then throwError $ TypeChecker pos $ ConstantAssign v
    else if t /= et
        then throwError $ TypeChecker pos $ WrongType v t [et]
    else pure (None, False)
checkTypeI (IRet _ e) = checkTypeE e >>= (\x -> pure (x, False)) . Definitive
checkTypeI (IRetUnit pos) = pure (Definitive VTVoid, False)
checkTypeI (IYield _ e) = undefined
checkTypeI (IYieldUnit _) = undefined
checkTypeI (IBreak _) = pure (None, True)
checkTypeI (ICont _) = pure (None, True)
checkTypeI (IIf pos eb bl) = checkTypeI (IIfElse pos eb bl (IBlock pos []))
checkTypeI (IIfElse pos eb bl1 bl2) = do
    eb' <- checkTypeE eb
    if eb' /= VTBool
        then throwError $ TypeChecker pos $ WrongTypeOp "if else condition" eb'
        else do
            (bl1Ret, loopFlow1) <- checkTypeB bl1
            (bl2Ret, loopFlow2) <- checkTypeB bl2
            let loopFlow = loopFlow1 || loopFlow2
            case (bl1Ret, bl2Ret) of
              (None, None) -> pure None
              (Definitive vt, None) -> pure $ Branching vt
              (None, Definitive vt) -> pure $ Branching vt
              (Branching vt, None)  -> pure $ Branching vt
              (None, Branching vt)  -> pure $ Branching vt
              (Definitive vt1, Definitive vt2) -> go vt1 vt2 Definitive
              (Branching  vt1, Branching  vt2) -> go vt1 vt2 Branching
              (Definitive vt1, Branching  vt2) -> go vt1 vt2 Branching
              (Branching  vt1, Definitive vt2) -> go vt1 vt2 Branching
              >>= \x -> pure (x, loopFlow)
            where
                go :: VarType -> VarType -> (VarType -> RetType') -> IM RetType'
                go vt1 vt2 f =
                    if vt1 == vt2
                        then pure $ f vt1
                        else throwError $ TypeChecker pos $ MismatchedReturnTypes vt1 vt2
checkTypeI (IWhile pos eb bl) = checkTypeI (IIf pos eb bl) >>= \(x, _) -> pure (x, False)
checkTypeI (IWhileFin pos eb (IBlock _ bIs) (IBlock _ finIs)) = do
    retType <- checkTypeI (IWhile pos eb $ IBlock pos bIs)
    finRetType <- checkTypeB (IBlock pos finIs)
    mergeRetTypes pos retType finRetType
checkTypeI (IFor pos v e1 e2 bl) = do
    n1 <- checkTypeE e1
    n2 <- checkTypeE e2
    if n1 /= VTInt || n2 /= VTInt
        then throwError $ TypeChecker pos $ ForRangeTypeMismatch n1 n2
        else localState (Map.insert v (VTInt, VMConst)) $ checkTypeI (IWhile pos (EBoolLitTrue pos) bl)
        >>= \(x, _) -> pure (x, False)
checkTypeI IForGen {} = undefined
checkTypeI (IBBlock _ b) = checkTypeB b
checkTypeI (DFun pos v args t b) = undefined -- TODO: implement


checkTypeB :: Block -> IM RetType
checkTypeB (IBlock _ is) = localState id $ checkTypeBHelper (None, False) is

--                  previous   next  
checkTypeBHelper :: RetType -> [Instr] -> IM RetType
checkTypeBHelper prev [] = pure prev
checkTypeBHelper prev (i:is) = do
    ir <- checkTypeI i
    let pos = hasPosition i
    merged <- mergeRetTypes pos prev ir
    checkTypeBHelper merged is


mergeRetTypes :: BNFC'Position -> RetType -> RetType -> IM RetType
mergeRetTypes pos (r1, l1) (r2, l2) = do
    let loopFlow = l1 || l2
    case (r1, r2) of
        (None, _) -> pure r2
        (_, None) -> pure r1
        (Branching  t1, Definitive t2) -> merge t1 t2 Definitive
        (Branching  t1, Branching  t2) -> merge t1 t2 Branching
        (Definitive t1, Branching  t2) -> merge t1 t2 Definitive
        (Definitive t1, Definitive t2) -> merge t1 t2 Definitive
        >>= \x -> pure (x, loopFlow)
    where
        merge :: VarType -> VarType -> (VarType -> RetType') -> IM RetType'
        merge t1 t2 f =
            if t1 == t2
                then pure $ f t1
                else throwError $ TypeChecker pos $ MismatchedReturnTypes t1 t2


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
checkTypeE (ENeg pos (ONeg _) e) = checkExprSingleOp "negation" pos e VTInt
checkTypeE (ENot pos (ONot _) e) = checkExprSingleOp "negation" pos e VTBool
checkTypeE (EMul pos e1 (ODiv _) (EIntLit _ 0)) = throwError $ TypeChecker pos ZeroLiternalDiv
checkTypeE (EMul pos e1 op e2) = checkExprBiOp (getMulOpName op) pos e1 e2 VTInt
checkTypeE (ESum pos e1 op e2) = checkExprBiOp (getSumOpName op) pos e1 e2 VTInt
checkTypeE (ERel pos e1 op e2) = checkExprBiOp (getRelOpName op) pos e1 e2 VTInt >> pure VTBool
checkTypeE (EBAnd pos e1 (OAnd _) e2) = checkExprBiOp "and" pos e1 e2 VTBool
checkTypeE (EBOr pos e1 (OOr _) e2) = checkExprBiOp "or" pos e1 e2 VTBool
checkTypeE (ETer pos eb e1 e2) = do
    t1 <- checkTypeE e1
    t2 <- checkTypeE e2
    tb <- checkTypeE eb
    if tb /= VTBool
        then throwError $ TypeChecker pos $ WrongTypeOp "ternary operator condition" tb
        else if t1 /= t2
            then throwError $ TypeChecker pos $ TernaryMismatch t1 t2
            else pure t1


getMulOpName :: MulOp -> String
getMulOpName (OMul _) = "multiplication"
getMulOpName (ODiv _) = "division"
getMulOpName (OMod _) = "modulo"

getSumOpName :: PlsOp -> String
getSumOpName (OPlus _)  = "addition"
getSumOpName (OMinus _) = "subtraction"

getRelOpName :: RelOp -> String
getRelOpName (REq _)  = "equality"
getRelOpName (RNeq _) = "inequality"
getRelOpName (RLt _)  = "less than"
getRelOpName (RGt _)  = "greater than"
getRelOpName (RLeq _) = "less or equal"
getRelOpName (RGeq _) = "greater or equal"


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
            VMMut -> pure (None, False)
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
    pure (None, False)


checkExprSingleOp :: String -> BNFC'Position -> Expr' BNFC'Position -> VarType -> IM VarType
checkExprSingleOp opN pos e t = do
    t' <- checkTypeE e
    if t == t'
        then pure t
        else throwError $ TypeChecker pos $ WrongTypeOp opN t'


checkExprBiOp :: String -> BNFC'Position -> Expr' BNFC'Position -> Expr' BNFC'Position -> VarType -> IM VarType
checkExprBiOp opN pos e1 e2 t = do
    t1 <- checkTypeE e1
    t2 <- checkTypeE e2
    if t1 == t && t2 == t
        then pure t
        else throwError $ TypeChecker pos $ WrongTypeBiOp opN t1 t2
