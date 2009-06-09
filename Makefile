all: exe

# @TODO FBR: use hmake to automate dependency maintenance
exe: clean
	ghc -W -Wall -c CommentRemoval.hs Main.hs
	ghc -W -Wall -o unpig CommentRemoval.o Main.o

clean:
	rm -f *.o *.hi unpig

tags:
	hasktags -e Main.hs CommentRemoval.hs
