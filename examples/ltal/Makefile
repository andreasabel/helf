# Makefile for LTAL 

helf = ../../bin/helf +RTS -sstderr -RTS
helf-prof = ../../bin/helf-prof +RTS -p -h -sstderr -RTS
big = w32_sig_semant.elf

all :
	for engine in Closures Ordered HerBruijn; do $(helf) -e $$engine $(big); done

6000 :
	for engine in Closures Ordered HerBruijn; do $(helf) -e $$engine 6000.elf; done



default : 	
	$(helf) $(big)

prof : 	
	$(helf-prof) $(big)

# EOF
