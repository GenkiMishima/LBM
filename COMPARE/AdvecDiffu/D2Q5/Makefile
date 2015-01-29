OUTPUT  :=main
FAST    :=-parallel -O3
DEBUG   :=-check -traceback
COMP    :=ifort

COMP :=$(COMP) $(DEBUG)
#COMP :=$(COMP) $(FAST)
HEAD = prmtr.o vrble.o
SUB  = condition.o FDM.o LBM.o output.o

all:$(OUTPUT)

prmtr.o: prmtr.f90
	$(COMP) -c $<
vrble.o: vrble.f90 prmtr.o
	$(COMP) -c $<
condition.o: condition.f90 $(HEAD)
	$(COMP) -c $<
FDM.o: FDM.f90 $(HEAD)
	$(COMP) -c $<
LBM.o: LBM.f90 $(HEAD)
	$(COMP) -c $<
output.o: output.f90 $(HEAD)
	$(COMP) -c $<
$(OUTPUT): $(OUTPUT).f90 $(HEAD) $(SUB)
	$(COMP) $^ -o $@

clean:
	rm -rf $(OUTPUT) a.out lbm *.o *.mod restart.bin vtk data/* *.png *.d
swp:
	rm -rf .*.swp
