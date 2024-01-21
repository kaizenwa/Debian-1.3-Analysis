# Makefile for GNU Pascal compiler.
#   Copyright (C) 1987, 1996 Free Software Foundation, Inc.
#
# This file is part of GNU Pascal.
#
# GNU Pascal is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 1, or (at your option)
# any later version.
#
# GNU Pascal is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Pascal; see the file COPYING.  If not, write to
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

.SUFFIXES: .p .pas .o .h

#.pas.o:
#	$(PC) -c $(PFLAGS) $(CPPFLAGS) -o $@ $<

PC = gpc
ALLPFLAGS = $(PFLAGS) -g -O3 --automake="-g -O3"
EXE =

TESTS = abso$(EXE) asmnames$(EXE) bitmanip$(EXE) caseelse$(EXE)		\
	cstparam$(EXE) head$(EXE) incdec$(EXE) iso$(EXE) kohl$(EXE)	\
	mem$(EXE) minimax$(EXE) nohead$(EXE) objects$(EXE)		\
	openarr$(EXE) switches$(EXE) varrec$(EXE) voidparm$(EXE)

TESTSRC = head.pas nohead.pas cstparam.pas caseelse.pas switches.pas	\
	bitmanip.pas incdec.pas minimax.pas mem.pas abso.pas		\
	asmnames.pas openarr.pas varrec.pas voidparm.pas

all: bptest

bptest:
	for x in $(TESTSRC) ; do \
	  rm -f a.out 2>/dev/null ; \
	  $(PC) $(ALLPFLAGS) $$x 2>/dev/null ; \
	  echo -n "$$x: " ; \
	  if [ -f "a.out" ] ; then ./a.out ; else echo "failed" ; fi ; \
	done ; \
	rm myunit.o myunit.gpi a.out 2>/dev/null ;

# DOS can't run shell script
dostest: $(TESTS)

clean:
	-rm -f $(TESTS)
	-rm -f *.o *.gpi *.gpc

distclean: clean
	-rm -f *~ core a.out #* *.BAK *.bak *.orig

# empty
install:

abso$(EXE): abso.pas
	$(PC) $(ALLPFLAGS) -o abso$(EXE) abso.pas

asmnames$(EXE): asmnames.pas
	$(PC) $(ALLPFLAGS) -o asmnames$(EXE) asmnames.pas

bitmanip$(EXE): bitmanip.pas
	$(PC) $(ALLPFLAGS) -o bitmanip$(EXE) bitmanip.pas

caseelse$(EXE): caseelse.pas
	$(PC) $(ALLPFLAGS) -o caseelse$(EXE) caseelse.pas

cstparam$(EXE): cstparam.pas
	$(PC) $(ALLPFLAGS) -o cstparam$(EXE) cstparam.pas

head$(EXE): head.pas
	$(PC) $(ALLPFLAGS) -o head$(EXE) head.pas

incdec$(EXE): incdec.pas
	$(PC) $(ALLPFLAGS) -o incdec$(EXE) incdec.pas

iso$(EXE): iso.pas
	$(PC) $(ALLPFLAGS) -o iso$(EXE) iso.pas

# :-)
kohl$(EXE): kohl.pas
	$(PC) $(ALLPFLAGS) -o kohl$(EXE) kohl.pas

mem$(EXE): mem.pas
	$(PC) $(ALLPFLAGS) -o mem$(EXE) mem.pas

minimax$(EXE): minimax.pas
	$(PC) $(ALLPFLAGS) -o minimax$(EXE) minimax.pas

nohead$(EXE): nohead.pas
	$(PC) $(ALLPFLAGS) -o nohead$(EXE) nohead.pas

objects$(EXE): objects.pas
	$(PC) $(ALLPFLAGS) -o objects$(EXE) objects.pas

openarr$(EXE): openarr.pas
	$(PC) $(ALLPFLAGS) -o openarr$(EXE) openarr.pas

switches$(EXE): switches.pas
	$(PC) $(ALLPFLAGS) -o switches$(EXE) switches.pas

varrec$(EXE): varrec.pas
	$(PC) $(ALLPFLAGS) -o varrec$(EXE) varrec.pas
