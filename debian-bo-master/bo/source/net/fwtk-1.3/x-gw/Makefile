#
#  Copyright (c) 1993, Trusted Information Systems, Incorporated
#  All rights reserved.
# 
#  Redistribution and use are governed by the terms detailed in the
#  license document ("LICENSE") included with the toolkit.
#

#
#	Author: WEI XU, Trusted Information Systems, Inc.
#
# RcsId: "Header: Makefile,v 1.2 94/11/01 12:04:53 mjr Exp "

include ../Makefile.config

CFLAGS= -I.. $(COPT) -I$(XINCLUDE)
TISLIB = ../libfwall.a ulib.a
XLIBS =  -L$(XLIBDIR) -lXaw -lXmu -lXt -lXext -lX11 

ULIB_OBJS = child.o fwd.o pmsg.o sig.o socket.o string.o sws.o \
	token.o dialog.o shell.o list.o setenv.o getenv.o
ULIB = ulib.a

all:	$(ULIB) x-gw

$(ULIB):$(ULIB_OBJS)
	ar rcv $@ $(ULIB_OBJS)
	$(RANLIB) $@

x-gw:	x-gw.o $(TISLIB)
	$(CC) $(LDFL) -o $@ x-gw.o $(TISLIB) $(XLIBS) $(AUXLIB)

clean:
	rm -f x-gw *.o ulib.a

install:	x-gw
	if [ -f $(DEST)/x-gw ]; then \
		mv $(DEST)/x-gw $(DEST)/x-gw.old; \
	fi
	$(CP) x-gw $(DEST)
	chmod 755 $(DEST)/x-gw
