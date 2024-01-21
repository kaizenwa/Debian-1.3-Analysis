#----------------------------------------------------------------------
# Makefile for SVGAlib demo programs.
#
# This file is a part of SVGAlib.
#----------------------------------------------------------------------

include ../Makefile.cfg

srcdir    = ..
VPATH     = $(srcdir)/demos

#----------------------------------------------------------------------
# Compiler Section (overrides Makefile.cfg)
#----------------------------------------------------------------------

CFLAGS = $(WARN) $(OPTIMIZE) -I$(srcdir)/src -I$(srcdir)/gl

#----------------------------------------------------------------------
# Rules Section
#----------------------------------------------------------------------

PROGS    = fun testgl speedtest mousetest vgatest scrolltest testlinear \
	   keytest testaccel accel forktest eventtest
PROGS_O  = fun.o testgl.o speedtest.o mousetest.o vgatest.o scrolltest.o \
	   testlinear.o keytest.o testaccel.o accel.o forktest.o eventtest.o
LIBS     = -lvgagl -lvga
#LIBS     = -lvgagl -lvga -lg

all:	progs
.PHONY: all clean cleanbin dep

progs : $(PROGS)

objs:	$(PROGS_O)

$(PROGS_O): .depend

.c.o:
	$(CC) $(CFLAGS) -c -o $*.o $<

.c.s:
	$(CC) $(CFLAGS) -S -o $*.s $<

.o:
	$(CC) $(CFLAGS) $(LDFLAGS) -o $* $*.o $(LIBS)
	chmod 4755 $*

testaccel: testaccel.o
	$(CC) $(CFLAGS) $(LDFLAGS) -o testaccel testaccel.o $(LIBS) -lm
	chmod 4755 testaccel

clean:	cleanbin
	rm -f .depend *.o *~

cleanbin:
	rm -f $(PROGS)

dep:
	rm -f .depend
	make .depend

.depend:
	echo '# Program dependencies' >.depend
	gcc -MM $(CFLAGS) $(patsubst %.o,$(srcdir)/demos/%.c,$(PROGS_O)) >>.depend

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif
