#----------------------------------------------------------------------
# Makefile for SVGAlib utilities.
#
# This file is a part of SVGAlib.
#----------------------------------------------------------------------

include ../Makefile.cfg

srcdir    = ..
VPATH     = $(srcdir)/utils

#----------------------------------------------------------------------
# Compiler Section (overrides Makefile.cfg)
#----------------------------------------------------------------------

CFLAGS = $(WARN) $(OPTIMIZE) -s
#Use the next one for the Alpha/AXP if you need it
#LDFLAGS = -L../src 
LIBS = -lvga

#----------------------------------------------------------------------
# Rules Section
#----------------------------------------------------------------------

UTILPROGS = restorefont convfont restoretextmode restorepalette dumpreg \
	fix132x43 setmclk
OBJECTS = restorefont.o convfont.o restoretextmode.o restorepalette.o \
	dumpreg.o fix132x43.o setmclk.o

.c.o:
	$(CC) $(CFLAGS) -c -o $*.o $<

.o:
	$(CC) $(CFLAGS) $(LDFLAGS) -o $* $*.o $(LIBS)
	chmod 4755 $*

all: $(UTILPROGS)
.PHONY: all clean cleanbin dep

$(OBJECTS): .depend

convfont: convfont.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o convfont convfont.o $(LIBS)

clean:
	rm -f $(UTILPROGS) *.o *~

dep:
	rm -f .depend
	make .depend

install:
	$(INSTALLPROG) $(UTILPROGS) $(UTILINSTALLDIR)

.depend:
	gcc -MM $(patsubst %.o,$(srcdir)/utils/%.c,$(OBJECTS)) >.depend

#
# include a dependency file if one exists
#
ifeq (.depend.src,$(wildcard .depend.src))
include .depend.src
endif
