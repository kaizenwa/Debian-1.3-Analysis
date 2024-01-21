#
# ibcs/Tools/Makefile 
#
# $Id: Makefile,v 1.9 1996/08/23 15:09:53 mike Exp $
# $Source: /usr/CVS/ibcs/Tools/Makefile,v $
#


all:		lnxstamp trace

install:	all

clean:
		rm -f trace lnxstamp


lnxstamp:	lnxstamp.c
		cc -O -fomit-frame-pointer -o lnxstamp lnxstamp.c

trace:		trace.c
		cc -O -fomit-frame-pointer -o trace -I../include trace.c

dep:
		$(CPP) -M -I../include $(CFLAGS) *.c > .depend

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif
