default: run

run:
	runhaskell main.hs

build:
	ghc main.hs
	rm *.hi
	rm *.o

