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
                       BNFC'Position, Ident (..), HasPosition (hasPosition)
                       )

import Src.Types ( VarRef )
import Src.Errors ( ErrHolder, Err (..), ErrHolder (RuntimeError), RuntimeType (..) )
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT)
import qualified Data.Map as Map (Map, empty, insert, lookup, fromList)

-- =============================================================================

newtype Env = Env { env :: Map.Map Ident Loc }

emptyEnv :: Env
emptyEnv = Env Map.empty

-- =============================================================================

type FnArg = (Ident, VarRef)
data Value
    = VTInt Int
    | VTBool Bool
    | VTString String
    | VTUnit
    | VTFun [FnArg] Block Env

type Loc = Int

data Store = Store { nextFree :: Loc, memory :: Map.Map Loc Value }

emptyStore :: Store
emptyStore = Store 0 Map.empty

newlock :: Store -> (Loc, Store)
newlock (Store n m) = (n, Store (n + 1) m)

insert :: Loc -> Value -> Store -> Store
insert l v (Store n m) = Store n (Map.insert l v m)

-- =============================================================================

--                  errors     local   env  modify memory io
type IM a = ExceptT ErrHolder (ReaderT Env (StateT Store IO)) a


evaluate :: Program -> IO (Err ())
evaluate p = do 
    let errorsT = runExceptT (evalP p)
    let envT = runReaderT errorsT emptyEnv
    evalStateT envT emptyStore


evalP :: Program -> IM ()
evalP _ = undefined
