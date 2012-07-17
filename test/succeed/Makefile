# Helf
# Makefile for successful tests
# Authors: Andreas Abel, Ulf Norell
# Created: 2004-12-03, 2008-09-03, 2010-11-19

helf = helf +RTS -K16M -RTS
# ../../bin/helf
# put helf in your path!

# Getting all lf files
excluded=./shared_forall2_i.elf
allelf=$(patsubst %.elf,%,$(filter-out $(excluded),$(shell find . -name "*.elf")))

all : $(allelf) 

$(allelf) : % : %.elf
	@echo "----------------------------------------------------------------------"
	@echo $<
	@echo "----------------------------------------------------------------------"
	@$(helf) $<

clean :
	-rm *~

#EOF
