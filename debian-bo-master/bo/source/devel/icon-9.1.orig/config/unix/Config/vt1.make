MAKE=make

all:		define
		cd common;  $(MAKE) getopt.o alloc.o filepart.o strtbl.o ipp.o
		cd itran;	$(MAKE) ../vitran;

define:		define.icn
		icont -s define.icn
