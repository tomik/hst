all: main

clean: 
	rm *.o *.hi *.prof hst

main: *.hs
	ghc -o hst -O2 --make Main.hs

prof: *.hs 
	ghc -o hst -prof -auto-all -O --make Main.hs

runp: hst
	./hst +RTS -p -RTS

