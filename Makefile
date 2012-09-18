
.PHONY: all clean

all: ghci

ghci: Ghci.hs
	ghc --make Ghci.hs -o ghci

clean:
	-rm *.hi *.o ghci

