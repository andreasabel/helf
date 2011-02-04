# Makefile for helf
# see src/Makefile

helf = helf +RTS -K16M -sstderr -RTS
time = gtime -v

.PHONY : test examples current default helf

default : 
	make -C src

bench : helf
	$(time) $(helf) examples/ltal/w32_sig_semant.elf

current : helf
	$(helf) test/succeed/word32_ltal_sig.elf

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
	-make -C src clean
	make -C test/fail clean

# EOF
