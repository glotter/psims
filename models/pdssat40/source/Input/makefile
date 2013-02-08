
OBJDIR = objCSMINPUT
FC     = gfortran  
FFLAGS = -static -O3 -g -fno-automatic -ffpe-trap=invalid,zero,overflow,underflow
INC    = -I.
MKDIR  = mkdir -p
DIRS   = ../Common ../Input 
RM     = rm -rf

# Create a list of all source files including folder name
FSRC   = $(foreach dir,$(DIRS),$(wildcard $(dir)/*.for))

## Create the list of all object files
TOBJ   = $(foreach dir, $(DIRS), $(subst .for,.o,$(notdir $(wildcard $(dir)/*.for))))

# Create the path to store the object files
FOBJ   = $(foreach obj,$(TOBJ),$(OBJDIR)/$(obj))

# Define the subfolders to look for source files
vpath %.for ../Common ../Input 

$(OBJDIR)/%.o $(OBJDIR)/%.mod: %.for | $(OBJDIR) 
	$(FC) $(FFLAGS) $(INC) -o $@ -c $<

../MINPT040.EXE : $(FOBJ)
	$(FC) $^ -static -o $@

all:
	echo $(FOBJ)

$(OBJDIR):
	-test -d $(OBJDIR) || $(MKDIR) $(OBJDIR)

# Create a list of dependencies
depend: $(FSRC)
	../sfmakedepend --objdir $(OBJDIR) --moddir $(OBJDIR) $(FSRC)

clean:
	$(RM) $(OBJDIR)/*.o
	$(RM) $(OBJDIR)/*.mod
	$(RM) *.mod
	$(RM) $(OBJDIR)



# DO NOT DELETE THIS LINE - used by make depend
objCSMINPUT/DATES.o: objCSMINPUT/ModuleDefs.o




objCSMINPUT/UTILS.o: objCSMINPUT/ModuleDefs.o

objCSMINPUT/Warning.o: objCSMINPUT/ModuleDefs.o

objCSMINPUT/INSOIL.o: objCSMINPUT/ModuleDefs.o



objCSMINPUT/INVAR.o: COMGEN.BLK

objCSMINPUT/IPCHEM.o: objCSMINPUT/ModuleDefs.o


objCSMINPUT/IPENV.o: objCSMINPUT/ModuleDefs.o

objCSMINPUT/IPMAN.o: objCSMINPUT/ModuleDefs.o

objCSMINPUT/IPSLIN.o: objCSMINPUT/ModuleDefs.o

objCSMINPUT/IPSOIL.o: COMSOI.BLK
objCSMINPUT/IPSOIL.o: objCSMINPUT/ModuleDefs.o

objCSMINPUT/IPTILL.o: objCSMINPUT/ModuleDefs.o

objCSMINPUT/IPVAR.o: COMGEN.BLK

objCSMINPUT/LMATCH.o: objCSMINPUT/ModuleDefs.o

objCSMINPUT/OPGEN.o: COMSWI.BLK COMSOI.BLK COMGEN.BLK
objCSMINPUT/OPGEN.o: objCSMINPUT/ModuleDefs.o

objCSMINPUT/OPHEAD.o: COMSWI.BLK COMIBS.BLK COMSOI.BLK
objCSMINPUT/OPHEAD.o: objCSMINPUT/ModuleDefs.o

objCSMINPUT/OPTEMPXY2K.o: COMSWI.BLK COMSOI.BLK COMIBS.BLK COMGEN.BLK
objCSMINPUT/OPTEMPXY2K.o: objCSMINPUT/ModuleDefs.o




objCSMINPUT/SEFERT.o: objCSMINPUT/ModuleDefs.o




objCSMINPUT/SEINIT.o: objCSMINPUT/ModuleDefs.o

objCSMINPUT/SEIRR.o: objCSMINPUT/ModuleDefs.o

objCSMINPUT/SENS.o: COMSWI.BLK COMSOI.BLK COMIBS.BLK
objCSMINPUT/SENS.o: objCSMINPUT/ModuleDefs.o



objCSMINPUT/SERES.o: objCSMINPUT/ModuleDefs.o


objCSMINPUT/SESOIL.o: objCSMINPUT/ModuleDefs.o






objCSMINPUT/input.o: COMSWI.BLK COMIBS.BLK COMSOI.BLK
objCSMINPUT/input.o: objCSMINPUT/ModuleDefs.o

objCSMINPUT/ipexp.o: COMSWI.BLK COMIBS.BLK
objCSMINPUT/ipexp.o: objCSMINPUT/ModuleDefs.o

objCSMINPUT/optempy2k.o: COMSWI.BLK COMSOI.BLK COMIBS.BLK COMGEN.BLK
objCSMINPUT/optempy2k.o: objCSMINPUT/ModuleDefs.o

floodmodule.mod: ModuleDefs.o
