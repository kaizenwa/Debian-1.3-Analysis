CPPFLAGS=-I..
CXXFLAGS=-O0 -g -Wall  
#Although I always test-compile this with -O0, the debian/rules file
#will call this makefile with CFLAGS=-O2, so your release is always optimised.

all: fvwmgen

fvwmgen.o:fvwmgen.cc ../common.old.h fvwmgen.h
	rm -f fvwmgen.o
	g++ $(CPPFLAGS) $(CXXFLAGS) -c  -o fvwmgen.o fvwmgen.cc

fvwmgen: fvwmgen.o ../common.old.o
	rm -f fvwmgen
	g++ -o fvwmgen $(CXXFLAGS) fvwmgen.o ../common.old.o

clean   :
	rm -f fvwmgen fvwmgen.o core
