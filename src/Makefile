# Makefile for helf

files=Main Lexer Parser Util Concrete Abstract OperatorPrecedenceParser Scoping ScopeMonad PrettyM Value Signature Context TypeCheck ListEnv MapEnv ClosVal Closures OrderedComplex2 OrderedCom2 HeredNormal HeredNormVal HerBruijnVal HerBruijn DatastrucImpl/SimpleDynArray DatastrucImpl/StrictDynArray TermGraph TGChecker NamedExplSubst Fresh ORef TermGraph TGChecker
# Warshall Util TreeShapedOrder TraceError Extract HsSyntax ToHaskell PrettyTCM
hsfiles=$(foreach file,$(files),$(file).hs)
# ghcflags=-fglasgow-exts -ignore-package monads-fd -O -rtsopts
ghcflags=-fglasgow-exts -ignore-package monads-fd -O -rtsopts
profflags=-prof -auto-all
.PHONY : test examples current default helf

default : helf test

current :
	helf test/succeed/z.elf

helf : ../bin/helf

../bin/helf : Main.hs $(hsfiles)
	ghc $(ghcflags) $< --make -o $@

# profiling binary

helf-prof : ../bin/helf-prof

../bin/helf-prof : Main.hs $(hsfiles)
	ghc $(ghcflags) $(profflags) $< --make -o $@


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
	-rm *.o *.hi ../bin/helf
#	make -C test/fail clean


# EOF
