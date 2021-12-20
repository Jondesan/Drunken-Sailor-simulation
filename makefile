# -*-makefile-*-
FC = gfortran
OBJ = main.o sailor_type.o position_type.o mtfort90.o ui.o
DEPS = mtfort90.f90 sailor_type.f90 position_type.f90 ui.f90 main.f90
EXE = drunken_sailor.exe

drunken_sailor.exe: $(DEPS)
	$(FC) -c $(DEPS)
	$(FC) -o $(EXE) $(OBJ)
	rm -f *.o
	./$(EXE)

run:
	./$(EXE)