import Src.Errors
import Src.Utils
import Src.Types
import Src.Jabba.Par
import Src.Jabba.Abs
import System.Directory.Internal.Prelude (exitFailure)
import Src.TypeChecker (typeCheck, typeCheckWithEnv, TypeEnv)
import qualified Data.Map as Map
import Debug.Trace (trace)

type FileTestCase = (String, ErrType)
type InlineTestCase = (String, TypeEnv, ErrType)

testFiles :: [FileTestCase]
testFiles = [
    ("12-001-const", ImmutVar (Ident "x")),
    ("12-002-multiname", VarAlreadyDecl (Ident "x")),
    ("12-003-wrong-type", WrongType (Ident "s") VTString [VTInt]),
    ("12-004-wrong-type-var", WrongType (Ident "s") VTString [VTInt]),
    ("12-005-multi-wrong-type", WrongType (Ident "x") VTInt [VTString]),
    ("12-006-multi-wrong-type-var", WrongType (Ident "x") VTInt [VTString]),
    ("12-007-undef", NotDeclVar (Ident "x")),
    ("12-008-undef-op", NotDeclVar (Ident "x")),
    ("12-009-boolen", WrongType (Ident "x") VTInt [VTBool]),
    ("12-010-boolen2", WrongType (Ident "x") VTString [VTBool])
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
        then putStrLn $ "Success " ++ show name
        else do
          putStrLn $ "Failure " ++ show name
          putStrLn $ "Expected: " ++ show expected
          putStrLn $ "Got: " ++ show err
          exitFailure
    Right _ -> do
      putStrLn $ "Failure " ++ show name
      putStrLn $ "Expected: " ++ show expected
      putStrLn $ "Got: " ++ "No error"
      exitFailure
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
        then putStrLn $ "Success " ++ show content
        else do
          putStrLn $ "Failure " ++ show content
          putStrLn $ "Expected: " ++ show expected
          putStrLn $ "Got: " ++ show err
          exitFailure
    Right _ -> do
      putStrLn $ "Failure " ++ show content
      putStrLn $ "Expected: " ++ show expected
      putStrLn $ "Got: " ++ "No error"
      exitFailure
  where
    go ts = do
      tree <- left ParserErr $ pProgram ts
      typeCheckWithEnv env tree
      pure ()


main :: IO ()
main = do
  mapM_ testSingleFileCase testFiles
  mapM_ testSingleInlineCase inlineTests
