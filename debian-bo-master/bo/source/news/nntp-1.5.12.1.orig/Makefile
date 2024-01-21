#
# Makefile for NNTP intstallation
# $Id: Makefile,v 1.11 1994/12/04 00:16:12 sob Exp sob $
# The program that knows how to deal with makefiles. [Uncomment if needed.]
# MAKE=make
# set CFLAGS to be -Ipath/to/special/includes if needed
CFLAGS= -O 
#Uncomment the following for Solaris (and similiar SVR4 systems)
#NETLIBS   = -lsocket -lnsl 
#Uncomment the following if the previous one does not work
#NETLIBS   = -lnsl -lnet -lsocket
# set DBLIBS to be -lneededlibrary if needed
#DBLIBS	= -ldbz
#DBLIBS	= /usr/local/lib/dbz.o #/usr/local/lib/dbzdbm.o
#DBLIBS	= -ldbm
# add the resolver library if needed
#RESLIB = -lresolv
# add the kmem library if needed 
#KVMLIB = -lkvm
#Uncomment the following for Solaris (and similiar SVR4 systems)
#KVMLIB = -lelf
# Where nntpd is going to live
ETCDIR = /etc
# Where nntpxmit and nntpxfer is going to live
BINDIR = /usr/local/bin
# Where manual pages live
MANDIR = /usr/local/man
#
LIBS = $(DBLIBS) $(RESLIB) $(NETLIBS) $(KVMLIB)
SUBMAKEFLAGS="LIBS=${LIBS}" "CFLAGS=${CFLAGS}" "ETCDIR=${ETCDIR}" "BINDIR=${BINDIR}" "MANDIR=${MANDIR}"

DISTFILES = Makefile README CHANGES

DISTDIRS = doc server support xfer xmit

DIST = $(DISTFILES) $(DISTDIRS)

SHELL=/bin/sh

TYPE = server

all: make_$(TYPE)

make_server: /nowhere
	cd server; $(MAKE) $(MFLAGS) $(SUBMAKEFLAGS)
	cd support; $(MAKE) $(MFLAGS) $(SUBMAKEFLAGS)
	cd doc; $(MAKE) $(MFLAGS) $(SUBMAKEFLAGS)
	cd xmit; $(MAKE) $(MFLAGS) $(SUBMAKEFLAGS)
	cd xfer; $(MAKE) $(MFLAGS) $(SUBMAKEFLAGS)

server: make_server

make_client: /nowhere
	@ echo "There is no client software in this release.  Get the"
	@ echo "NNTP client kit version 1.6 or greater."

client: make_client

install: install_$(TYPE)

install_server:
	cd server; $(MAKE) $(MFLAGS) $(SUBMAKEFLAGS) install
	cd support; $(MAKE) $(MFLAGS) $(SUBMAKEFLAGS) install
	cd doc; $(MAKE) $(MFLAGS) $(SUBMAKEFLAGS) install
	cd xmit; $(MAKE) $(MFLAGS) $(SUBMAKEFLAGS) install
	cd xfer; $(MAKE) $(MFLAGS) $(SUBMAKEFLAGS) install

install_msgidd:
	cd server; $(MAKE) $(MFLAGS) $(SUBMAKEFLAGS) install_msgidd

clean:
	rm  -f .[a-z]* *.CKP *.BAK *~ ; for i in $(DISTDIRS); do cd $$i; $(MAKE) $(MFLAGS) clean; cd ..; done

check:
	for i in $(DISTDIRS); do cd $$i; $(MAKE) $(MFLAGS) check; cd ..; done

distrib: check
	mkdir ../nntp.dist
	cp -rp $(DIST) ../nntp.dist
	cd ../nntp.dist; for i in $(DISTDIRS); do cd $$i; $(MAKE) $(MFLAGS) distrib; cd ..; done


server: /nowhere

/nowhere: ;
