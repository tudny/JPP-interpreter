
import Src.Jabba.Abs   ()
import Src.Jabba.Lex   ( Token, mkPosToken )
import Src.Jabba.Par   ( pProgram, myLexer )
import Src.Jabba.Print ( Print, printTree )
import Src.Jabba.Skel  ()

main :: IO ()
main = do 
    putStrLn "Welcome to the interpreter!"
