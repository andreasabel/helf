# Makefile for helf

files=Main Lexer Parser Common Concrete 
# Abstract ScopeChecker TypeChecker Value TCM Eval Termination SPos 
# Warshall Util TreeShapedOrder TraceError Extract HsSyntax ToHaskell PrettyTCM
hsfiles=$(foreach file,$(files),$(file).hs)
ghcflags=-fglasgow-exts

.PHONY : test examples current default

default : helf test

current : helf
	helf test/succeed/comments.elf

helf : ../bin/helf

../bin/helf : Main.hs $(hsfiles)
	ghc $(ghcflags) $< --make -o $@

Lexer.hs : Lexer.x
	alex $<

%arser.hs : %arser.y Lexer.hs
	happy --info=$<-grm.txt $<

test : helf         
	@echo "======================================================================"
	@echo "===================== Suite of successfull tests ====================="
	@echo "======================================================================"
	make -C ../test/succeed
	@echo "======================================================================"
	@echo "======================= Suite of failing tests ======================="
	@echo "======================================================================"
	make -C ../test/fail

examples : helf
	@echo "======================================================================"
	@echo "========================== Suite of examples ========================="
	@echo "======================================================================"
	make -C ../examples


clean : 
	rm *.o *.hi
	make -C test/fail clean


# EOF
