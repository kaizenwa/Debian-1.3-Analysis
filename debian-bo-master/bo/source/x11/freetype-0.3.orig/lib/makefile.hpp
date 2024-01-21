# This file is part of the FreeType project.
#
# It builds the library and test programs for gcc (and X11).
#
# You will need GNU make.

CC= cc
#CFLAGS= -Wall -pedantic -ansi -O2 -g 
CFLAGS= -Aa -g 

SRC=  ttcalc.c    ttfile.c    tttables.c ttraster.c    \
      tterror.c   ttmemory.c  ttindex.c


OBJ=  ttcalc.o    ttfile.o    tttables.o ttraster.o   \
      tterror.o   ttmemory.o  ttindex.o

all: libttf.a

libttf.a: $(OBJ) 
	$(AR) rc $@ $^

clean:
	-$(RM) $(OBJ) 

distclean: clean
	$(RM) dep.end libttf.a

depend:
	$(CC) -E -M $(SRC) > dep.end

ifeq (dep.end,$(wildcard dep.end))
  include dep.end
endif
