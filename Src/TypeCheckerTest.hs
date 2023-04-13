import Src.Errors
import Src.Utils
import Src.Types
import Src.Jabba.Par
import Src.Jabba.Abs
import System.Directory.Internal.Prelude (exitFailure)
import Src.TypeChecker (typeCheck, typeCheckWithEnv, TypeEnv)
import qualified Data.Map as Map

type FileTestCase = (String, ErrType)
type InlineTestCase = (String, TypeEnv, ErrType)

testFiles :: [FileTestCase]
testFiles = [
    ("12-001-const", ImmutVar (Ident "x"))
  ]

inlineTests :: [InlineTestCase]
inlineTests = [
    (
      "x++;", 
      Map.fromList [(Ident "x", (VTInt, VMConst))], 
      ImmutVar (Ident "x")
    )
  ]


checkErrorMatch :: ErrType -> ErrHolder -> Bool
checkErrorMatch t h = 
    case h of
        TypeChecker _ t' -> t == t'
        _ -> False

testSingleFileCase :: FileTestCase -> IO ()
testSingleFileCase (name, expected) = do
  fileContent <- readFile ("bad/" ++ name ++ ".jbb")
  let ts = myLexer fileContent
  case go ts of
    Left err -> do
      if checkErrorMatch expected err
        then putStrLn $ "Success" ++ show name
        else do
          putStrLn $ "Failure " ++ show name
          exitFailure
    Right _ -> putStrLn "Success"
  where
    go ts = do
      tree <- left ParserErr $ pProgram ts
      typeCheck tree
      pure ()


testSingleInlineCase :: InlineTestCase -> IO ()
testSingleInlineCase (content, env, expected) = do
  let ts = myLexer content
  case go ts of
    Left err -> do
      if checkErrorMatch expected err
        then putStrLn $ "Success" ++ show content
        else do
          putStrLn $ "Failure " ++ show content
          exitFailure
    Right _ -> putStrLn "Success"
  where
    go ts = do
      tree <- left ParserErr $ pProgram ts
      typeCheckWithEnv env tree
      pure ()


main :: IO ()
main = do
  mapM_ testSingleFileCase testFiles
  mapM_ testSingleInlineCase inlineTests
