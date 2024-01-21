#=======================================================================
#  Makefile for example V draw program
#  Copyright (C) 1995  Bruce E. Wampler
#
#  This program is part of the V C++ GUI Framework example programs.
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#=======================================================================

CONFIG=../Config.mk
include $(CONFIG)

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

PROG=\
	$(Bin)/vdraw

EXOBJS=\
	$(oDir)/vdrwapp.o \
	$(oDir)/vdrwcnv.o \
	$(oDir)/vdrwcmdw.o \
	$(oDir)/vdrwdlg.o

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

.PHONY: default all objs clean cleanobj cleanall

default: all

all: $(PROG)

clean:
	-rm -f $(CLEANEXTS)

cleanobj: clean
	-rm -f $(EXOBJS)

cleanall: cleanobj
	-rm -f $(PROG)

objs:	$(EXOBJS)

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

$(PROG):	$(EXOBJS)
	$(CXX) -o $@ $(EXOBJS) $(LDFLAGS)

$(oDir)/vdrwcnv.o:	vdrwcnv.cpp v_defs.h vdrwcnv.h vdrwcmdw.h vdrwdlg.h \
	vpsdc.h vprinter.h
	$(CXX) -c $(CFLAGS) -o $@ $<                     

$(oDir)/vdrwdlg.o:	vdrwdlg.cpp v_defs.h vdrwdlg.h vdrwcmdw.h vdrwcnv.h
	$(CXX) -c $(CFLAGS) -o $@ $<                     

$(oDir)/vdrwcmdw.o:	vdrwcmdw.cpp v_defs.h vdrwcmdw.h vdrwcnv.h vdrwdlg.h
	$(CXX) -c $(CFLAGS) -o $@ $<                     

$(oDir)/vdrwapp.o:	vdrwapp.cpp v_defs.h vdrwapp.h vdrwcmdw.h vdrwcnv.h vdrwdlg.h
	$(CXX) -c $(CFLAGS) -o $@ $<

