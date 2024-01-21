CPPFLAGS=-I..
CXXFLAGS=-O0 -g -Wall -fhandle-exceptions   
#Although I test-compile this with -O0, the debian/rules file
#will call this makefile with CFLAGS=-O2, so your release is always optimised.

all: menu-method

menu-method.o: menu-method.h menu-method.cc
	rm -f menu-method.o
	g++ $(CPPFLAGS) $(CXXFLAGS) -c  -o menu-method.o menu-method.cc

menu-method: menu-method.o ../adstring.o
	rm -f menu-method
	g++ -o menu-method $(CXXFLAGS) menu-method.o ../adstring.o

static: menu-method.o ../adstring.o
	rm -f menu-method
	g++ -o static $(CXXFLAGS) menu-method.o ../adstring.o\
	     -lc -Wl,-static -lstdc++

adstring.o:../adstring.h ../adstring.cc
	make -C .. adstring.o 

clean   :
	rm -f menu-method.o menu-method core static 2

backup : clean
	tar -cvzf $$GM0HOME/menu.tar.gz *
