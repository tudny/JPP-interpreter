
all:

clean:

generate:
	bnfc -m -d --haskell -p Parser -o src lang/jabba.cf
