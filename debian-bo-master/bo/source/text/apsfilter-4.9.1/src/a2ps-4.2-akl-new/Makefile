#
# Description: Makefile to construct and install a2ps. Don't forget
# to give your own values to define the variable name of compiler
# (see OPTIONS).
#
# File: imag:/users/local/src/a2ps/Makefile
# Created: Mon Dic 21 18:25:08 1992 by santana@imag.fr (Miguel Santana)
# Version: 4.2
#

# Copyright (c) 1992, 1993, Miguel Santana, santana@imag.fr
#
# Permission is granted to copy and distribute this file, for noncommercial
# use, provided (a) this copyright notice is preserved, (b) no attempt is
# made to restrict redistribution of this file, and (c) this file is
# not distributed as part of any collection whose redistribution is
# restricted by a compilation copyright.
#

SHELL	=/bin/sh

#CC	=gcc
CC	=cc

D	=.
O	=.

MANDIR	=/usr/man/man1
MANEXT	=1

#CFLAGS	= -s -pipe -m486 -O2 -fomit-frame-pointer -DBSD -DAKL # -DAKL_DEBUG
CFLAGS	= -s -O -DBSD -DAKL # -DAKL_DEBUG

# Default values for HEIGHT, WIDTH, MARGIN and DIR_SEP (11.64, 8.27, 1.2, '/').
# Default values for LPR_PRINT, LPR_COMMAND and LPR_OPT (TRUE, "lpr", "-l")
# No values for TWOSIDED_DFLT, ONESIDED and TWOSIDED
OBJS	      = a2ps.o

all:		xa2ps

xa2ps:		a2ps.o
		@echo "Linking a2ps ... "
		$(CC) -o $D/xa2ps a2ps.o

a2ps.o:		a2ps.c
		@echo "Compiling a2ps ... "
		$(CC) -c $(CFLAGS) a2ps.c

install:	xa2ps
		@echo "Installing a2ps ..."
		./install_it

install.man:	a2ps.1
		@echo "Installing a2ps man page ..."
		cp a2ps.1 $(MANDIR)/a2ps.$(MANEXT)
		chmod a+r $(MANDIR)/a2ps.$(MANEXT)

shar:;		shar -b -oa2ps.shar README Makefile a2ps.c a2ps.1

tar:;		tar cvf a2ps.tar README Makefile a2ps.c a2ps.1

clean:;		rm -f $(OBJS) xa2ps core *~ *O
