# This file is part of the FreeType project.
#
# It builds the library and test programs for gcc (and X11).
#
# You will need GNU make.

MAKEFILE= makefile.gcc

CC= gcc
# X11DIRS= -I/usr/openwin/include
X11LIBS= -L/usr/X11R6/lib
# X11LIBS= -L/usr/openwin/lib
INCDIRS= -Ilib $(X11DIRS)

CFLAGS= -Wall -O2 -ansi -g $(INCDIRS) -DX11

SRC=  zoom.c timer.c win_x11.c

OBJ=  win_x11.o
OBJ1= zoom.o
OBJ2= timer.o


all: freetype timer zoom

freetype:
	cd lib; $(MAKE) -f $(MAKEFILE) all

zoom: $(OBJ) $(OBJ1) lib/libttf.a
	$(CC) $(CFLAGS) -o $@ $^ -lm $(X11LIBS) -lX11 

timer: $(OBJ) $(OBJ2) lib/libttf.a
	$(CC) $(CFLAGS) -o $@ $^ -lm $(X11LIBS) -lX11

clean: do_clean
	cd lib; $(MAKE) -f $(MAKEFILE) clean

distclean: do_clean
	cd lib; $(MAKE) -f $(MAKEFILE) distclean
	-$(RM) dep.end zoom timer

do_clean:
	-$(RM) $(OBJ) $(OBJ1) $(OBJ2)

depend:
	cd lib; $(MAKE) -f $(MAKEFILE) depend
	$(CC) -E -M $(INCDIRS) $(SRC) > dep.end

ifeq (dep.end,$(wildcard dep.end))
  include dep.end
endif
