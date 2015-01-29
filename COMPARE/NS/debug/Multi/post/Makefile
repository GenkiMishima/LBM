OUTPUT  :=postprocess
FAST    :=-fast -parallel
DEBUG   :=-check -traceback
COMP    :=ifort

COMP :=$(COMP) $(DEBUG)

OBJ = prmtr.o post_prmtr.o

all:$(OUTPUT)

prmtr.o: ../prmtr.f90
	$(COMP) -c $<
post_prmtr.o: post_prmtr.f90 prmtr.o
	$(COMP) -c $<

$(OUTPUT): $(OUTPUT).f90 $(OBJ)
	$(COMP) $^ -o $(OUTPUT)

vtk: postprocess.f90 $(core)
	$(COMP) $^ -o vtk
clean:
	rm -rf $(OUTPUT) a.out lbm *.o *.mod restart.bin vtk result/* result/.*swp
