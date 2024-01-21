#
#  Copyright (c) 1993, Trusted Information Systems, Incorporated
#  All rights reserved.
# 
#  Redistribution and use are governed by the terms detailed in the
#  license document ("LICENSE") included with the toolkit.
#

#
#	Author: Marcus J. Ranum, Trusted Information Systems, Inc.
#
# RcsId: "Header: Makefile,v 1.3 94/11/01 11:51:55 mjr rel "


include	../Makefile.config


#if you are using the SKEY modules, define SKEYDIR to be the source
#directory where the SKEY libraries and include files are.
#SKEYDIR=../../skey
#SKEYINC= -I$(SKEYDIR)
#SKEYLIB= $(SKEYDIR)/libskey.a
#SKEYOBJ= skey.o


#if you are using the SecurID module, define SECURDIR to be the source
#directory where the SecurID libraries and include files are.
#SECURDIR=../../secid/master/sun4clien
#SECURLIB= /home/mjr/secid/master/sdconf.o /home/mjr/secid/master/sdclient.a
#SECURINC= -I$(SECURDIR)
#SECUROBJ= securid.o


#if you are using the Enigma Logic module, define ENIGMADIR to be the source
#directory where the Enigma Logic libraries and include files are.
#ENIGMADIR=/u/b/mjr/hacks/enigma
#ENIGMALIB=$(ENIGMADIR)/idpb.a
#ENIGMAINC= -I$(ENIGMADIR)
#ENIGMAOBJ= enigma.o


#if you are using the Digital Pathways SNK module, include this:
#this also requires DES routines below
#SNKOBJ=	snk.o


#if you are using link-level encryption code, this should be the path
#to a library that contains DES routines (NOT SUPPLIED!)
#you will need to replace srvio.c and cliio.c
#DESLIB=	../../libdes/libdes.a
#DESINC=	-I../../libdes


#security protocol modules
MODULES= $(SKEYLIB) $(SECURLIB) $(DESLIB)

CFLAGS= -I.. $(COPT)
LIB=	../libauth.a
FWLIB=	../libfwall.a

OBJ=	cliio.o
SRVOBJ= proto.o db.o pass.o srvio.o \
	$(SNKOBJ) $(SKEYOBJ) $(SECUROBJ) $(ENIGMAOBJ)

all:	$(LIB) authmgr authsrv authdump authload
$(LIB): $(OBJ)
	ar rcv $@ $(OBJ)
	$(RANLIB) $@

authmgr:	$(LIB) $(FWLIB) authmgr.o
	$(CC) $(LDFL) -o $@ authmgr.o $(LIB) $(FWLIB) $(DESLIB) $(AUXLIB)

authsrv:	$(LIB) $(FWLIB) authsrv.o $(SRVOBJ)
	$(CC) $(LDFL) -o $@ authsrv.o $(SRVOBJ) $(LIB) $(FWLIB) $(MODULES) $(DBMLIB) $(AUXLIB)
	chmod 755 $@

authdump:	$(LIB) $(FWLIB) authdump.o db.o
	$(CC) $(LDFL) -o $@ authdump.o db.o $(FWLIB) $(DBMLIB) $(AUXLIB)

authload:	$(LIB) $(FWLIB) authload.o db.o
	$(CC) $(LDFL) -o $@ authload.o db.o $(FWLIB) $(DBMLIB) $(AUXLIB)

snkkey:	snkkey.o
	$(CC) $(LDFL) -o $@ snkkey.o $(DESLIB) $(AUXLIB)

proto.o: proto.c ../auth.h
	$(CC) $(CFLAGS) $(SKEYINC) $(SECURINC) -c proto.c

cliio.o: cliio.c
	$(CC) $(CFLAGS) $(DESINC) -c cliio.c

srvio.o: srvio.c
	$(CC) $(CFLAGS) $(DESINC) -c srvio.c

snk.o: snk.c
	$(CC) $(CFLAGS) $(DESINC) -c snk.c

snkkey.o: snkkey.c
	$(CC) $(CFLAGS) $(DESINC) -c snkkey.c

skey.o: skey.c
	$(CC) $(CFLAGS) $(SKEYINC) -c skey.c

securid.o: securid.c
	$(CC) $(CFLAGS) $(SECURINC) -c securid.c

clean:
	rm -f $(LIB) *.o authmgr authsrv authdump authload snkkey

install:	all
	if [ -f $(DEST)/authsrv ]; then \
		mv $(DEST)/authsrv $(DEST)/authsrv.old; \
	fi
	$(CP) authsrv $(DEST)
	chmod 755 $(DEST)/authsrv
	if [ -f $(DEST)/authmgr ]; then \
		mv $(DEST)/authmgr $(DEST)/authmgr.old; \
	fi
	$(CP) authmgr $(DEST)
	chmod 755 $(DEST)/authmgr
	if [ -f $(DEST)/authload ]; then \
		mv $(DEST)/authload $(DEST)/authload.old; \
	fi
	$(CP) authload $(DEST)
	chmod 755 $(DEST)/authload
	if [ -f $(DEST)/authdump ]; then \
		mv $(DEST)/authdump $(DEST)/authdump.old; \
	fi
	$(CP) authdump $(DEST)
	chmod 755 $(DEST)/authdump
