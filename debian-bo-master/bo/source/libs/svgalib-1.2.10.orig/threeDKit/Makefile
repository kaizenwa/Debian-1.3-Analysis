#----------------------------------------------------------------------
# Makefile for plane and wrapdemo. See README file.
#
# This file is a part of SVGAlib.
#----------------------------------------------------------------------

include ../Makefile.cfg

srcdir    = ..
VPATH     = $(srcdir)/threeDKit

#----------------------------------------------------------------------
# Compiler Section (overrides Makefile.cfg)
#----------------------------------------------------------------------

DLLFLAGS  =

#----------------------------------------------------------------------
# Rules Section
#----------------------------------------------------------------------

OBJECTS = 3dinit.o 3dkit.o triangle.o striangle.o\
	  wtriangle.o swtriangle.o wrapsurf.o
LIBS = -lm -lvga -lvgagl

.PHONY: clean

all: plane wrapdemo

plane: planukit.o planinit.o $(OBJECTS)
	$(CC) $(LDFLAGS) -o plane planukit.o planinit.o $(OBJECTS) $(LIBS)
	chmod u+s plane

wrapdemo: wrapdemo.o $(OBJECTS)
	$(CC) -o wrapdemo wrapdemo.o $(OBJECTS) $(LIBS)
	chmod u+s wrapdemo

.c.o:
	$(CC) $(CFLAGS) -c -o $*.o $<

clean:
	rm -f *.o plane wrapdemo
