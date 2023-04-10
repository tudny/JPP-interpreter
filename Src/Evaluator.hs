module Src.Evaluator where

import Src.Jabba.Abs ( Program )

type Err        = Either String

evaluate :: Program -> Err (IO ())
evaluate p = Right $ putStrLn "Hello World!"
