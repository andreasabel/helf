# Helf
# Makefile for successful tests
# Authors: Andreas Abel, Ulf Norell
# Created: 2004-12-03, 2008-09-03, 2010-11-19

helf = helf
# ../../bin/helf
# put helf in your path!

# Getting all lf files
allelf=$(patsubst %.elf,%,$(shell find . -name "*.elf"))

all : $(allelf) 

$(allelf) : % : %.elf
	@echo "----------------------------------------------------------------------"
	@echo $<
	@echo "----------------------------------------------------------------------"
	@$(helf) $<

clean :
	-rm *~

#EOF
