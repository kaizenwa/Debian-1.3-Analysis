#
# NET-BASE	A collection of programs that form the base set of the
#		NET-2 Networking Distribution for the LINUX operating
#		system.  This Makefile creates the Support Library.
#
# Version:	@(#)Makefile.l	1.20	12/12/93
#
# Author:	Fred N. van Kempen, <waltje@uwalt.nl.mugnet.org>
#		Copyright 1993 MicroWalt Corporation
#
#		This program is free software; you can redistribute it
#		and/or  modify it under  the terms of  the GNU General
#		Public  License as  published  by  the  Free  Software
#		Foundation;  either  version 2 of the License, or  (at
#		your option) any later version.
#

HWOBJS	= hw.o loopback.o slip.o ether.o ax25.o ppp.o arcnet.o
AFOBJS	= af.o unix.o inet.o ax25.o ipx.o ddp.o ipx.o
VARIA	= getsock.o getargs.o
NLSMISC = net-string.o

OBJS	= $(NLSMISC) $(VARIA) $(AFOBJS) $(HWOBJS)


libsupport.a:	Makefile ../config.h $(OBJS)
		@rm -f libsupport.a
		@ar rcs libsupport.a $(OBJS)
		@ranlib libsupport.a

clean:
		rm -f *.o

clobber:	clean
		rm -f *.a

# End of Makefile.
