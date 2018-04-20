SHELL=/bin/bash
FC=gfortran
CC=gcc
INCLUDE=-I./trrlib/include
FLAGS=-O

OBJECTS= \
        trrlib/lib/xdrfile.o \
        trrlib/lib/xdrfile_trr.o \
        trrlib/lib/xdrfile_xtc.o \
	trrlib/xdr.o \
        compute_k.o
        
compile:$(OBJECTS)
	$(FC) $(FLAGS) $^ -o compute_k $(LIBS) $(INCLUDE)

%.o:%.f90
	$(FC) $(FLAGS) -c $< -o $@

%.o:%.c
	$(CC) $(FLAGS) $(INCLUDE) -c $< -o $@

clean:
	rm $(OBJECTS) *.mod


dist:
	if [ -e kinetic_trr.zip ]; then  \
	    rm kinetic_trr.zip;          \
	fi
	if [ -d kinetic_trr ]; then  \
	    rm kinetic_trr -r;           \
	fi
	# Add sources to build objects
	for file in $(OBJECTS); do            \
	    f=$${file/.o/.f90};               \
	    if [ -e $$f ]; then               \
	        zip kinetic_trr.zip $$f; \
	    fi;                               \
	    f=$${file/.o/.c};                 \
	    if [ -e $$f ]; then               \
	        zip kinetic_trr.zip $$f; \
	    fi;                               \
	done
	# Add this Makefile
	zip kinetic_trr.zip Makefile
	# Add headers (for C code)
	for file in $$(find . -name *.h); do  \
	    zip kinetic_trr.zip $$file;  \
	done
	# Now create a folder and rezip
	mkdir kinetic_trr
	mv kinetic_trr.zip kinetic_trr
	cd kinetic_trr;                    \
	unzip kinetic_trr.zip;             \
	rm kinetic_trr.zip;                \
	cd -
	zip kinetic_trr.zip kinetic_trr -r
	rm kinetic_trr -r


