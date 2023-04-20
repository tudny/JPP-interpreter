## File generated by the BNF Converter (bnfc 2.9.4.1).
## And edited by hand by Aleksander Tudruj.

# Makefile for building the parser and test program.

GHC        = ghc
GHC_OPTS   = -odir Target -hidir Target -Wall -Werror -Wextra # -O2
HAPPY      = happy 
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

PREFIX 	   = /home/tudny/.local/bin

SRCS =\
	Src/Errors.hs\
	Src/Evaluator.hs\
	Src/Interpreter.hs\
	Src/TypeChecker.hs\
	Src/TypeCheckerTest.hs\
	Src/Types.hs\
	Src/Utils.hs\


# List of goals not corresponding to file names.

.PHONY : all clean distclean

# Default goal.

all : Interpreter Test TypeCheckerTest Makefile


# Rules for building the parser.

Src/Jabba/Abs.hs Src/Jabba/Lex.x Src/Jabba/Par.y Src/Jabba/Print.hs Src/Jabba/Test.hs : Lang/jabba.cf
	bnfc --haskell -p Src -d Lang/jabba.cf --functor

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

Interpreter : Src/Jabba/Abs.hs Src/Jabba/Lex.hs Src/Jabba/Par.hs Src/Jabba/Print.hs Src/Interpreter.hs $(SRCS)
	${GHC} ${GHC_OPTS} Src/Interpreter -o Interpreter

Test : Src/Jabba/Abs.hs Src/Jabba/Lex.hs Src/Jabba/Par.hs Src/Jabba/Print.hs Src/Jabba/Test.hs $(SRCS)
	${GHC} ${GHC_OPTS} Src/Jabba/Test -o Test

TypeCheckerTest : Src/Jabba/Abs.hs Src/Jabba/Lex.hs Src/Jabba/Par.hs Src/Jabba/Print.hs Src/TypeCheckerTest.hs $(SRCS)
	${GHC} ${GHC_OPTS} Src/TypeCheckerTest -o TypeCheckerTest

install : Interpreter
	install Interpreter $(PREFIX)/jabba

test : all
	./check_examples.sh

# Rules for cleaning generated files.

clean :
	-rm -rf Target/

distclean : clean
	-rm -f Interpreter
	-rm -f Test
	-rm -f TypeCheckerTest

# EOF
