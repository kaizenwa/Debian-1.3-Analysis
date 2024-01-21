# Copyright (c) 1991 Bell Communications Research, Inc. (Bellcore)
# 
# Permission to use, copy, modify, and distribute this material 
# for any purpose and without fee is hereby granted, provided 
# that the above copyright notice and this permission notice 
# appear in all copies, and that the name of Bellcore not be 
# used in advertising or publicity pertaining to this 
# material without the specific, prior written permission 
# of an authorized representative of Bellcore.  BELLCORE 
# MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR SUITABILITY 
# OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED "AS IS", 
# WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
#
FOOBAR-sun4=-Bstatic

VERS=2.3.6

STATICFLAG=FOOBAR${HOST_ARCH}

CFLAGS = -g -I.  ${$(STATICFLAG)} 

LDLIBS =

CONFIGDIR = ..

LOCALCFLAGS= -I${CONFIGDIR} ${CFLAGS}

.c.o:
	rm -f $@
	$(CC) -c  $(LOCALCFLAGS) $*.c

RICHOBJS=richlex.o richset.o usascii.o iso2022.o

all:  richtext richtoatk

richtext: richtext.o $(RICHOBJS)
	$(CC) ${LOCALCFLAGS} -o richtext richtext.o $(RICHOBJS) -ltermcap $(LDLIBS)

richtoatk: richtoatk.o $(RICHOBJS)
	$(CC) ${LOCALCFLAGS} -o richtoatk richtoatk.o $(RICHOBJS) $(LDLIBS)

richtext.o:	richlex.h richset.h
richtoatk.o:	richlex.h
richlex.o:	richlex.h richset.h
richset.o:	richlex.h richset.h
usascii.o:	richlex.h richset.h
iso2022.o:	richlex.h richset.h

clean:
	-rm -f richtext richtoatk *.o *.BAK

tar:
	tar cvf richtext-$(VERS).tar *.c *.h richtext.1 Makefile COMMANDS NOTES
