all: main

clean: 
	rm *.o *.hi hst

main: *.hs
	ghc -o hst -O2 --make Main.hs

