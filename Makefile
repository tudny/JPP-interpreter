## File generated by the BNF Converter (bnfc 2.9.4.1).
## And edited by hand by Aleksander Tudruj.

# Makefile for building the parser and test program.

GHC        = ghc
GHC_OPTS   = -odir Target -hidir Target
HAPPY      = happy 
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

# List of goals not corresponding to file names.

.PHONY : all clean distclean

# Default goal.

all : Interpreter Test TypeCheckerTest

Interpreter : Src/Interpreter
Test : Src/Jabba/Test
TypeCheckerTest : Src/TypeCheckerTest

# Rules for building the parser.

Src/Jabba/Abs.hs Src/Jabba/Lex.x Src/Jabba/Par.y Src/Jabba/Print.hs Src/Jabba/Test.hs : Lang/jabba.cf
	bnfc --haskell -p Src -d lang/jabba.cf

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

Src/Interpreter : Src/Jabba/Abs.hs Src/Jabba/Lex.hs Src/Jabba/Par.hs Src/Jabba/Print.hs Src/Interpreter.hs
	${GHC} ${GHC_OPTS} $@ -o Interpreter

Src/Jabba/Test : Src/Jabba/Abs.hs Src/Jabba/Lex.hs Src/Jabba/Par.hs Src/Jabba/Print.hs Src/Jabba/Test.hs
	${GHC} ${GHC_OPTS} $@ -o Test

Src/TypeCheckerTest : Src/Jabba/Abs.hs Src/Jabba/Lex.hs Src/Jabba/Par.hs Src/Jabba/Print.hs Src/TypeCheckerTest.hs
	${GHC} ${GHC_OPTS} $@ -o TypeCheckerTest

# Rules for cleaning generated files.

clean :
	-rm -rf Target/

distclean : clean
	-rm -f Interpreter
	-rm -f Test

# EOF
