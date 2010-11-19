# Makefile for helf
# see src/Makefile

.PHONY : test examples current default

default : 
	make -C src

current : helf
	helf test/succeed/comments.elf

helf : 
	make -C src helf

test : helf         
	@echo "======================================================================"
	@echo "===================== Suite of successfull tests ====================="
	@echo "======================================================================"
	make -C test/succeed
	@echo "======================================================================"
	@echo "======================= Suite of failing tests ======================="
	@echo "======================================================================"
	make -C test/fail

examples : helf
	@echo "======================================================================"
	@echo "========================== Suite of examples ========================="
	@echo "======================================================================"
	make -C examples


clean : 
	rm *.o *.hi
	make -C test/fail clean

# EOF
