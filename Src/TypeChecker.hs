module Src.TypeChecker where

import Src.Jabba.Abs ( Program )

type Err        = Either String

typeCheck :: Program -> Err Program
typeCheck p = Right p
