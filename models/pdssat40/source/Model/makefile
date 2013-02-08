
OBJDIR = objCSMMODEL
FC     = gfortran  
FFLAGS = -static -O3 -g -fno-automatic -ffpe-trap=invalid,zero,overflow,underflow 
INC    = -I../Input/
MKDIR  = mkdir -p
DIRS   = ../Common ../Model 
RM     = rm -rf

# Create a list of all source files including folder name
FSRC   = $(foreach dir,$(DIRS),$(wildcard $(dir)/*.for))

## Create the list of all object files
TOBJ   = $(foreach dir, $(DIRS), $(subst .for,.o,$(notdir $(wildcard $(dir)/*.for))))

# Create the path to store the object files
FOBJ   = $(foreach obj,$(TOBJ),$(OBJDIR)/$(obj))

# Define the subfolders to look for source files
vpath %.for ../Common ../Model 

$(OBJDIR)/%.o $(OBJDIR)/%.mod: %.for | $(OBJDIR) 
	$(FC) $(FFLAGS) $(INC) -o $@ -c $<

../DSSAT040.EXE : $(FOBJ)
	$(FC) $^ -static -o $@
	
all:
	echo $(FSRC)

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
