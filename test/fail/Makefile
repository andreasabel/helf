# helf
# Makefile for failing tests
# Author: Andreas Abel
# Created: 2004-12-06, 2008-09-03, 2010-11-19

# How this file works
# ===================
#
# Whenever a .elf file is modified,
# a corresponding .err file is generated to save the model error message
# for this file.  When the test suite is processed the next time, e.g.,
# after some hacking on the HELF implementation, the new error message
# is compared to the saved one.  If they do not match, this is considered
# an error.  Then one has to verify the new error message is actually the
# intended one (manually), and remove the .err file.

# Enable read -n and 2>&1 |.
SHELL=bash

helf=helf

# Getting all elf files
allelf=$(shell find . -name '*.elf')
allstems=$(patsubst %.elf,%,$(allelf))
allout=$(patsubst %.elf,%.err,$(allelf))

.PHONY : $(allstems)

default : all
all : $(allstems)

debug :
	@echo $(allelf)

# No error recorded

$(allout) : %.err : %.elf
	@echo "----------------------------------------------------------------------"
	@echo "$*.elf"
	@echo "----------------------------------------------------------------------"
	@if $(helf) $(shell if [ -e $*.flags ]; then cat $*.flags; fi) $< 2>&1 > $*.tmp; \
		then echo "Unexpected success"; rm -f $*.tmp; false; \
    else if [ -s $*.tmp ]; \
				 then sed -e "s/[^ ]*test.fail.//g" $*.tmp > $@; cat $@; rm -f $*.tmp; true; \
				 else rm -f $@ $*.tmp; false; \
				 fi; \
		fi

# Existing error


#				 echo `cat $*.err` > $*.tmp.2; \
#				 echo `cat $*.tmp` > $*.tmp.3; \

# NO WITH SPACES AFTER \ AT END OF LINE

$(allstems) : % : %.err
	@echo "----------------------------------------------------------------------"
	@echo "$*.elf"
	@echo "----------------------------------------------------------------------"
	@if $(helf) $(shell if [ -e $*.flags ]; then cat $*.flags; fi) $*.elf \
		 2>&1 > $*.tmp.2; \
		then echo "Unexpected success"; rm -f $*.tmp.2; false; \
    else sed -e "s/[^ ]*test.fail.//g" $*.tmp.2 > $*.tmp; \
				 echo `tail -1 $*.err` > $*.tmp.2; \
				 echo `tail -1 $*.tmp` > $*.tmp.3; \
				 true; \
		fi
	@if cmp $*.tmp.2 $*.tmp.3; \
	   then if cmp $*.tmp $*.err; \
                   then rm -f $*.tmp $*.tmp.2 $*.tmp.3; true; \
                   else mv $*.tmp $*.err; \
                        rm -f $*.tmp.2 $*.tmp.3; true; \
                fi; \
	   else echo "== Old error ==="; \
		cat $*.err; \
		echo "== New error ==="; \
		cat $*.tmp; \
		/bin/echo -n "Accept new error [y/N]? "; \
		read -n 1; \
		echo ""; \
		if [ "fckShPrg$$REPLY" != "fckShPrgy"  ]; \
                  then echo "Keeping old error"; false; \
		  else echo "Replacing error, continuing..."; \
                    mv $*.tmp $*.err; \
		    rm -f $*.tmp.2 $*.tmp.3; true; \
                fi; \
	    fi

# CAUTION: NO SPACE AFTER \
# RETARDED!!!!!!!

#		echo rm -f $*.tmp; echo rm -f $*.tmp.2; \
#		false;

# Clean

clean :
	-rm -f *.err *.tmp *.tmp.* *~ adm/*.err adm/*.tmp*

# EOF
