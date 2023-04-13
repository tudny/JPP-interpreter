module Src.Evaluator where

import Src.Jabba.Abs ( Program )
import Src.Errors ( ErrHolder )

type Err        = Either ErrHolder

evaluate :: Program -> Err (IO ())
evaluate p = Right $ putStrLn "Hello World!"
