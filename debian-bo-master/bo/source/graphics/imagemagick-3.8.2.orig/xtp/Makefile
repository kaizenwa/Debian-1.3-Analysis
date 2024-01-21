# Generated automatically from Makefile.in by configure.
#
#  Generic makefile for xtp for computers that do not have xmkmf.
#
#  Copyright 1997 E. I. du Pont de Nemours and Company
#
#  Permission to use, copy, modify, distribute, and sell this software and
#  its documentation for any purpose is hereby granted without fee,
#  provided that the above Copyright notice appear in all copies and that
#  both that Copyright notice and this permission notice appear in
#  supporting documentation, and that the name of E. I. du Pont de Nemours
#  and Company not be used in advertising or publicity pertaining to
#  distribution of the software without specific, written prior
#  permission.  E. I. du Pont de Nemours and Company makes no representations
#  about the suitability of this software for any purpose.  It is provided
#  "as is" without express or implied warranty.
#
#  E. I. du Pont de Nemours and Company disclaims all warranties with regard
#  to this software, including all implied warranties of merchantability
#  and fitness, in no event shall E. I. du Pont de Nemours and Company be
#  liable for any special, indirect or consequential damages or any
#  damages whatsoever resulting from loss of use, data or profits, whether
#  in an action of contract, negligence or other tortious action, arising
#  out of or in connection with the use or performance of this software.
#

CC = gcc
CDEBUGFLAGS = -g -O2
DEFS =  -DSTDC_HEADERS=1 -DHAVE_DIRENT_H=1 -DHAVE_SYS_WAIT_H=1 -DTIME_WITH_SYS_TIME=1 -DSVR4=1 
INSTALL = ./../install-sh -c
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_PROGRAM = ${INSTALL}
LDFLAGS =
LIBS = -lsocket  -lm
X_INCLUDES =  -I/usr/openwin/include
X_LIBRARIES =  -L/usr/openwin/lib -R/usr/openwin/lib -lsocket  -lnsl -lX11

prefix = /usr/local
exec_prefix = ${prefix}
LOCALDIR = ${exec_prefix}/bin
LOCALMAN = ${prefix}/man/mann

XTPObjects= xtp.o network.o regular.o

CFLAGS= $(CDEBUGFLAGS) $(DEFS) $(X_INCLUDES)

SYS_LIBRARIES= $(X_LIBRARIES) $(LIBS)

PROGRAMS= xtp 

all: $(PROGRAMS)

xtp: 	$(XTPObjects)
	rm -f $@
	$(CC) -o $@ $(XTPObjects) $(SYS_LIBRARIES)

clean::
	rm -f xtp

install:: xtp
	$(INSTALL_PROGRAM) xtp $(LOCALDIR)/xtp
	$(INSTALL_DATA) xtp.man $(LOCALMAN)/xtp.n

clean::
	rm -f *.ln *.bak *.o core errs ,* *~ *.a .emacs_* make.log MakeOut
