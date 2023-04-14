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
    ),
    (
      "f(1);",
      Map.fromList [(Ident "f", (VTFun [] VTInt, VMConst))],
      WrongNumberOfArgs (Ident "f") 0 1
    ),
    (
      "f();",
      Map.fromList [(Ident "f", (VTFun [(VTInt, VMConst, VRRef)] VTInt, VMConst))],
      WrongNumberOfArgs (Ident "f") 1 0
    ),
    (
      "f(1, 2, 3, 4, 5, 6);",
      Map.fromList [(Ident "f", (VTFun [(VTInt, VMConst, VRRef)] VTInt, VMConst))],
      WrongNumberOfArgs (Ident "f") 1 6
    ),
    (
      "f(\"abc\");",
      Map.fromList [(Ident "f", (VTFun [(VTInt, VMConst, VRRef)] VTInt, VMConst))],
      WrongTypeArg 0 VTString VTInt
    ),
    (
      "f(1, \"abc\");",
      Map.fromList [(Ident "f", (VTFun [(VTInt, VMConst, VRCopy), (VTInt, VMConst, VRCopy)] VTInt, VMConst))],
      WrongTypeArg 1 VTString VTInt
    ),
    (
      "f(x);",
      Map.fromList [(Ident "f", (VTFun [(VTInt, VMConst, VRRef)] VTInt, VMConst))],
      NotDeclVar (Ident "x")
    ),
    (
      "f(10);",
      Map.fromList [(Ident "f", (VTFun [(VTInt, VMMut, VRRef)] VTInt, VMConst))],
      ExprMutPass 0
    ),
    (
      "f(x);",
      Map.fromList [
        (Ident "f", (VTFun [(VTInt, VMMut, VRRef)] VTInt, VMConst)), -- f(var x: Integer) -> Integer
        (Ident "x", (VTInt, VMConst)) -- val x: Integer
        ],
      ImmutMutPass 0
    ),
    (
      "f(x + 1);",
      Map.fromList [
        (Ident "f", (VTFun [(VTInt, VMMut, VRRef)] VTInt, VMConst)), -- f(var x: Integer) -> Integer
        (Ident "x", (VTInt, VMConst)) -- val x: Integer
        ],
      ExprMutPass 0
    ),
    (
      "f(x + 0);",
      Map.fromList [
        (Ident "f", (VTFun [(VTInt, VMMut, VRRef)] VTInt, VMConst)), -- f(var x: Integer) -> Integer
        (Ident "x", (VTInt, VMConst)) -- val x: Integer
        ],
      ExprMutPass 0
    ),
    (
      "-\"abc\";",
      Map.empty,
      WrongTypeOp "negation" VTString
    ),
    (
      "!\"abc\";",
      Map.empty,
      WrongTypeOp "negation" VTString
    ),
    (
      "-true;",
      Map.empty,
      WrongTypeOp "negation" VTBool
    ),
    (
      "!1;",
      Map.empty,
      WrongTypeOp "negation" VTInt
    ),
    (
      "1+\"abc\";",
      Map.empty,
      WrongTypeBiOp "addition" VTInt VTString
    ),
    (
      "1-\"abc\";",
      Map.empty,
      WrongTypeBiOp "subtraction" VTInt VTString
    ),
    (
      "\"abc\"+1;",
      Map.empty,
      WrongTypeBiOp "addition" VTString VTInt
    ),
    (
      "\"abc\"-1;",
      Map.empty,
      WrongTypeBiOp "subtraction" VTString VTInt
    ),
    (
      "\"abc\"*1;",
      Map.empty,
      WrongTypeBiOp "multiplication" VTString VTInt
    ),
    (
      "\"abc\"/1;",
      Map.empty,
      WrongTypeBiOp "division" VTString VTInt
    ),
    (
      "\"abc\"%1;",
      Map.empty,
      WrongTypeBiOp "modulo" VTString VTInt
    ),
    (
      "\"abc\"<1;",
      Map.empty,
      WrongTypeBiOp "less than" VTString VTInt
    ),
    (
      "\"abc\">1;",
      Map.empty,
      WrongTypeBiOp "greater than" VTString VTInt
    ),
    (
      "\"abc\"<=1;",
      Map.empty,
      WrongTypeBiOp "less or equal" VTString VTInt
    ),
    (
      "\"abc\">=1;",
      Map.empty,
      WrongTypeBiOp "greater or equal" VTString VTInt
    ),
    (
      "\"abc\"==1;",
      Map.empty,
      WrongTypeBiOp "equality" VTString VTInt
    ),
    (
      "\"abc\"!=1;",
      Map.empty,
      WrongTypeBiOp "inequality" VTString VTInt
    ),
    (
      "\"abc\"&&1;",
      Map.empty,
      WrongTypeBiOp "and" VTString VTInt
    ),
    (
      "\"abc\"||1;",
      Map.empty,
      WrongTypeBiOp "or" VTString VTInt
    ),
    (
      "1+true;",
      Map.empty,
      WrongTypeBiOp "addition" VTInt VTBool
    ),
    (
      "1-true;",
      Map.empty,
      WrongTypeBiOp "subtraction" VTInt VTBool
    ),
    (
      "true+1;",
      Map.empty,
      WrongTypeBiOp "addition" VTBool VTInt
    ),
    (
      "true-1;",
      Map.empty,
      WrongTypeBiOp "subtraction" VTBool VTInt
    ),
    (
      "true*1;",
      Map.empty,
      WrongTypeBiOp "multiplication" VTBool VTInt
    ),
    (
      "true/1;",
      Map.empty,
      WrongTypeBiOp "division" VTBool VTInt
    ),
    (
      "true%1;",
      Map.empty,
      WrongTypeBiOp "modulo" VTBool VTInt
    ),
    (
      "true<1;",
      Map.empty,
      WrongTypeBiOp "less than" VTBool VTInt
    ),
    (
      "true>1;",
      Map.empty,
      WrongTypeBiOp "greater than" VTBool VTInt
    ),
    (
      "true<=1;",
      Map.empty,
      WrongTypeBiOp "less or equal" VTBool VTInt
    ),
    (
      "true>=1;",
      Map.empty,
      WrongTypeBiOp "greater or equal" VTBool VTInt
    ),
    (
      "true==1;",
      Map.empty,
      WrongTypeBiOp "equality" VTBool VTInt
    ),
    (
      "true!=1;",
      Map.empty,
      WrongTypeBiOp "inequality" VTBool VTInt
    ),
    (
      "true?1:\"abc\";",
      Map.empty,
      TernaryMismatch VTInt VTString
    ),
    (
      "true?1:\"abc\";",
      Map.empty,
      TernaryMismatch VTInt VTString
    ),
    (
      "var x: Boolean = (a < b); z;",
      Map.fromList [
        (Ident "a", (VTInt, VMConst)),
        (Ident "b", (VTInt, VMConst))
        ],
      NotDeclVar (Ident "z")
    ),
    (
      "var x: Boolean = (!b); z;",
      Map.fromList [
        (Ident "b", (VTBool, VMConst))
        ],
      NotDeclVar (Ident "z")
    ),
    (
      "(x && y || z) ? 1 : 2;",
      Map.fromList [
        (Ident "x", (VTBool, VMConst)),
        (Ident "y", (VTBool, VMConst)),
        (Ident "z", (VTInt, VMConst))
        ],
      WrongTypeBiOp "or" VTBool VTInt
    ),
    (
      "(x && y) ? \"ERROR\" : 1;",
      Map.fromList [
        (Ident "x", (VTBool, VMConst)),
        (Ident "y", (VTBool, VMConst))
        ],
      TernaryMismatch VTString VTInt
    ),
    (
      "1 ? 2 : 3;",
      Map.empty,
      WrongTypeOp "ternary operator condition" VTInt
    ),
    (
      "x = 1;",
      Map.fromList [
        (Ident "x", (VTInt, VMConst))
        ],
      ConstantAssign (Ident "x")
    ),
    (
      "x = 1;",
      Map.fromList [
        (Ident "x", (VTString, VMMut))
        ],
      WrongType (Ident "x") VTString [VTInt]
    ),
    (
      "val x: Integer = 1; x = 2;",
      Map.empty,
      ConstantAssign (Ident "x")
    ),
    (
      "var x: String = \"abc\"; x = 2;",
      Map.empty,
      WrongType (Ident "x") VTString [VTInt]
    ),
    (
      "var x: Boolean = true; x = 2;",
      Map.empty,
      WrongType (Ident "x") VTBool [VTInt]
    ),
    (
      "var x: Integer = 1; x = \"abc\";",
      Map.empty,
      WrongType (Ident "x") VTInt [VTString]
    ),
    (
      "var x: Integer = 1; x = true;",
      Map.empty,
      WrongType (Ident "x") VTInt [VTBool]
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
        then putStrLn $ "Success " ++ "\"" ++ take 40 content ++ "\""
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
  putStrLn "Running tests..."
  putStrLn $ "Detected files number: " ++ show (length testFiles)
  putStrLn $ "Detected inline tests number: " ++ show (length inlineTests) 
  mapM_ testSingleFileCase testFiles
  mapM_ testSingleInlineCase inlineTests
