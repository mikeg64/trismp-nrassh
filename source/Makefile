#
# Tristan makefile
#

SHELL=/bin/sh





cc= gcc
#cc= mpicc 

#FC= /usr/local/packages5/hdf5/1.8.10/bin/h5pfc
#LD= /usr/local/packages5/hdf5/1.8.10/bin/h5pfc


#FC= /usr/local/packages5/hdf5-1.8.9_pgi1.4.4/bin/h5pfc
#LD= /usr/local/packages5/hdf5-1.8.9_pgi1.4.4/bin/h5pfc


#Mit FC= /usr/local/packages5/hdf5-1.8.9_openmpigcc1.4.4/bin/h5pfc
#Mit LD= /usr/local/packages5/hdf5-1.8.9_openmpigcc1.4.4/bin/h5pfc

FC= /usr/local/packages6/hdf5/hdf5-1.8.9/bin/h5pfc
LD= /usr/local/packages6/hdf5/hdf5-1.8.9/bin/h5pfc

INCPATH= -I/usr/local/mpi/gcc/openmpi/1.4.4/include

LIBPATH = -L/usr/local/mpi/gcc/openmpi/1.4.4/lib -L/usr/local/packages6/hdf5/hdf5-1.8.9/lib -lmpi -lmpi_f90  -lmpi_f77 -lhdf5_fortran -lhdf5

#INCPATH= -I/usr/local/packages5/hdf5-1.8.9_openmpigcc1.4.4/include

USER=user_weibel
#USER=user_shock

# compiler and linker flags
# for debuggin purposes use these flags: (compiler dependent) 
#-check uninit -check pointers 
#-warn uncalled -warn declarations -ftrapuv -g -warn unused

# flags normaly used for the intel compilers

CUSTOM= -DMPI -DHDF5 -DserIO -DtwoD 
#-DLOCALSCRATCH 
#-DLOCALRESTART
#-DDEBUG
PERFORMANCE= -O3

# flags normaly used for the gcc and gfortran compilers

#CUSTOM= -DMPI -DHDF5 -DserIO -DtwoD -DWOODHEN0 
#PERFORMANCE= -O3 -cpp -fno-range-check -ffree-line-length-none

CFLAGS= 
FFLAGS= $(CUSTOM) $(PERFORMANCE)

# Executable name

EXECUTABLE= tristan-mp2d

# Objects

OBJS= system.o systemf.o par.o inputparser.o inputparserf.o fparser.o globaldata.o aux.o communications.o fields.o fieldboundaries.o particles.o domain.o $(USER).o output.o restart.o particles_movedeposit.o overload.o  initialize.o tristanmainloop.o tristan.o

###########################

all: $(EXECUTABLE)

%.o:%.c
	$(cc) $(CFLAGS) $(INCPATH) -c $^

%.o:%.F90
	$(FC) $(FFLAGS) $(INCPATH) -c $^

$(EXECUTABLE): $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS)
	tar -cf code.tar *.c *.F90 Makefile Makefile3D

clean: 
	rm -f $(OBJS)
	rm -f *.mod
	rm -f $(EXECUTABLE)
	rm -f code.tar
