#
# $Id: Makefile,v 1.4 1996/05/17 19:37:07 kenh Exp $
#
# Makefile for the Tk Netscape-remote extension
#
# Tampered with by Christian Bolik for inclusion into TkDesk 96/07/09.
#

#
# The following options will probably need adjustmend for your system
#

# The C compiler.  Must be ANSI
CC=gcc -Wall				# SunOS 4.x, NetBSD
#CC=cc					# IRIX

# Linker command
LD=ld					# Probably most everybody

# Flag to compile PIC (position-independent code) on your system
PICFLAG=-fPIC				# SunOS 4.x (with gcc), NetBSD
#PICFLAG=				# IRIX

# Location of X and Tcl/Tk include files
INCLUDE=-I/afs/cmf/@sys/include				# NRL only
#INCLUDE=-I/usr/X11R6/include -I/usr/local/include	# NetBSD

# Extension used by shared libraries on your system
SHAREDLIBEXTENSION=.so			# SunOS 4.x, NetBSD, IRIX

# Extra flags needed for ld to generate a shared library
SHAREDLIBFLAGS=-assert pure-text	# SunOS 4.x
#SHAREDLIBFLAGS=-Bshareable		# NetBSD
#SHAREDLIBFLAGS=-shared -rdata_shared	# IRIX

OPT=-O2					# Production
#OPT=-g					# Debugging

#
# You shouldn't need to adjust anything below
#

CFLAGS=$(OPT) $(PICFLAG) $(INCLUDE)
PROG=netscape-remote
OBJS=$(PROG).o ClientWin.o

all: $(PROG)$(SHAREDLIBEXTENSION)

$(PROG)$(SHAREDLIBEXTENSION): $(OBJS)
	$(LD) $(SHAREDLIBFLAGS) $(PROG).o ClientWin.o -o $@

libnetscape.a: $(OBJS)
	ar rc $@ $(OBJS)
	-ranlib $@

.c.o:
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o *.a $(PROG)$(SHAREDLIBEXTENSION)
