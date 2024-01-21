#
# Makefile for yppasswd/yppasswdd
# 
# Copyright 1994,1995 by Olaf Kirch, <okir@monad.swb.de>.
# Shadow support added by Charles Lopez.
# Various fixes by Peter Tobias.
# Support for non-Linux platforms by Dan Stromberg
#

CFLAGS  = $(OSFLGS) $(OSDEFS) $(DEFINES)

# OS-Dependent setup. You can define the following OS-dependent macros:
# NO_PATHS_H	if your system doesn't have <paths.h>
# NO_GETOPT_H	if your system doesn't have <getopt.h>. A working getopt()
#		is assumed, though.

# Linux
OSFLGS	= -Wall -O2 -fomit-frame-pointer
OSDEFS	= -D_GNU_SOURCE
CC	= gcc
XTRA	=

# Solaris 2.4/2.5
#OSFLGS	= -O2
#OSDEFS	=
#CC	= gcc
#XTRA	= -lsocket -lnsl

# Irix 5.2/5.3
#OSFLGS	= -O2
#OSDEFS	=
#CC	= gcc
#XTRA	= -lrpcsvc

# OSF/1 3.2c
#OSFLGS	= 
#OSDEFS	=
#CC	= cc
#XTRA	=

# Where to install everything. yppasswd goes to BINDIR, the daemon goes
# to SBINDIR, and the update script goes to LIBDIR. The traditional
# place for the client is /bin, but for FSSTND compliance, you may want
# to use /usr/bin.
BINDIR	= /usr/bin
SBINDIR	= /usr/sbin
LIBDIR	= /usr/lib/yp
MANDIR	= /usr/man

# If you want to use NYS instead of the old-style NIS code in libc,
# uncomment these lines
#NYSDEF	= -DNYS
#NYSLIB	= -lnsl

# For shadow password support, uncomment these lines
#SPWDEF	= -DSHADOWPWD
#SPWLIB	= -lshadow

# To enable proactive password checking with obvious(), uncomment these:
#OBVDEF	= -DUSE_OBVIOUS
#OBVOBJ	= obvious.o

# Combine defines and libs....
DEFINES = $(NYSDEF) $(SPWDEF) $(OBVDEF) \
	  -DYPLIBDIR=\"$(LIBDIR)\"
LIBS	= $(NYSLIB) $(SPWLIB) $(XTRA)

SRVROBJ = yppasswdd.o yppasswd_xdr.o update.o
CLNTOBJ = yppasswd.o  yppasswd_xdr.o $(OBVOBJ)
OBJ	= $(SRVROBJ) $(CLNTOBJ)
CLIENTS	= yppasswd
SERVER	= yppasswdd 
PROGS	= $(SERVER) $(CLIENTS) 
MAN1	= yppasswd.1 ypchsh.1 ypchfn.1
MAN8	= yppasswdd.8

all:	version.h $(PROGS)

yppasswd: $(CLNTOBJ)
	$(CC) $(LDFLAGS) -o $@ $(CLNTOBJ) $(LIBS)

yppasswdd: $(SRVROBJ)
	$(CC) $(LDFLAGS) -o $@ $(SRVROBJ) $(LIBS)

$(OBJ):: yppasswd.h

version.h: .version
	echo "#define VERSION \"`cat .version`\"" > $@

clean distclean::
	$(RM) core $(OBJ)

distclean::
	$(RM) -f $(PROGS)

install:: $(PROGS)
	@echo "Installing... "
	install -o root -g root -m 755 -s $(CLIENTS) $(DEBDIR)$(BINDIR)
	ln -sf yppasswd $(DEBDIR)$(BINDIR)/ypchfn
	ln -sf yppasswd $(DEBDIR)$(BINDIR)/ypchsh
	install -o root -g root -m 755 -s $(SERVER) $(DEBDIR)$(SBINDIR)/rpc.yppasswdd
	install -o root -g root -m 755 -d $(DEBDIR)$(LIBDIR)
	install -o root -g root -m 755 pwupdate $(DEBDIR)$(LIBDIR)

install installman::
	@echo "Installing manpages... "
	install -o root -g root -m 644 $(MAN8) $(DEBDIR)$(MANDIR)/man8
	install -o root -g root -m 644 $(MAN1) $(DEBDIR)$(MANDIR)/man1

