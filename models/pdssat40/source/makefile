# Main makefile

TRGTS = INPUT MODEL

all: $(TRGTS)

INPUT:
	cd ./Input; make

MODEL:
	cd ./Model; make

clean:
	rm -rf *.o *.mod 
	cd ./Input; make clean
	cd ./Model; make clean	
