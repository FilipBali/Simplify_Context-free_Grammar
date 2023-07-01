# Author: Filip Bali
# Date: 2022
# File: Makefile

# Note: If "split" library is missing, then run command "cabal install split --lib"


all:
	ghc -Wall --make -o simplify-bkg src/*.hs 
	
clean:
	rm src/*.o src/*.hi
