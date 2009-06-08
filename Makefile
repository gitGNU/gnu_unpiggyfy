all: exe tags

exe: clean
	ghc -W -Wall cmt_removal.hs -o unpig

clean:
	rm -f *.o *.hi unpig

tags:
	hasktags -e cmt_removal.hs
