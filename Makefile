all:
	ghc --make -O2 GMMRunner.hs
clean:
	rm *.o *.hi
