#
# Makefile
#
# This is the Makefile for libpvf.
#
# $Id: Makefile,v 1.8 1997/02/20 06:51:48 marc Exp $
#

PVFOBJS=au.o fft.o lib.o linear.o rockwell.o voc.o wav.o zyxel.o

.c.o:
	$(CC) $(CFLAGS) -c $*.c

all: libpvf.a

libpvf.a: $(PVFOBJS)
	$(RM) -f libpvf.a
	$(AR) $(ARFLAGS) libpvf.a $(PVFOBJS)
	-$(RANLIB) libpvf.a

install:

clean:
	@$(RM) -f libpvf.a $(PVFOBJS)

fullclean: clean

include depend
