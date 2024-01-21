# Copyright 1996 University Corporation for Atmospheric Research
#
# PC Makefile for common makefile rules used by the netCDF package.
#
# This file is configured for EMX's E3 method of building stand-alone,
# multi-threaded programs.  Information on this method can be found in
# the EMX documentation.

BISON		= bison
FLEX		= flex

F2C		= f2c
F2CFLAGS	= -A -g
F2CLIBS		= -lf2c

CC		= gcc
CFLAGS		= -Zomf -Zsys -Zmt
CPPFLAGS	= -DOS2 -DNO_SYSTEM_XDR_INCLUDES $(DEFINES) $(INCLUDES)
CXX		= gcc -x c++
CXXFLAGS	= -Zomf -Zsys

AR		= emxomfar
ARFLAGS		= rc

LDFLAGS		= -Zlinker /PM:VIO -Zstack 64000 -s

CAT		= cat
CP		= cp
RM		= rm
MKDIR		= mkdir
# `echo N | comp' is an alternative to the following:
DIFF		= diff


.SUFFIXES:
.SUFFIXES:	.exe .obj .i .c .f .cc

# Translate FORTRAN source code into C source code:
.f.c:
	$(F2C) $(F2CFLAGS) $< >$@

# Compile FORTRAN source code:
.f.obj:
	$(F2C) $(F2CFLAGS) $<
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $*.c
	$(RM) $*.c

# Compile C source code:
.c.obj:
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $<

# Preprocess C source code:
.c.i:
	$(CC) -E $(CPPFLAGS) $< >$@

# Compile C++ source code:
.cc.obj:
	$(CXX) -c $(CXXFLAGS) $(CPPFLAGS) $<

# Link an object into an executable:
.obj.exe:
	$(CC) -o $@ $(CFLAGS) $(LDFLAGS) $< $(OBJS) $(LIBS)


$(PROGRAM)tab.c \
$(PROGRAM)tab.h:	$(PROGRAM).y $(PROGRAM)yy.c $(PROGRAM).h
	$(BISON) -d -o $@ $(PROGRAM).y

$(DESTDIR):
	$(MKDIR) $@

$(BINDIR) \
$(LIBDIR) \
$(INCDIR):	$(DESTDIR)
	-$(MKDIR) $@

$(PROGRAM)yy.c:		$(PROGRAM).l
	$(FLEX) -t $(PROGRAM).l > $@

$(LIBRARY):		$(LIBOBJS) FORCE
	$(AR) $(ARFLAGS) $@ $(LIBOBJS)

$(LIBDIR)/$(LIBRARY):	$(LIBDIR) $(LIBRARY)
	$(CP) $(LIBRARY) $(LIBDIR)

$(INCDIR)/$(HEADER):	$(INCDIR) $(HEADER)
	$(CP) $(HEADER) $(INCDIR)

$(PROGRAM).exe:		$(OBJS)
	$(CC) -o $@ $(CFLAGS) $(LDFLAGS) $(OBJS) $(LIBS)

$(BINDIR)/$(PROGRAM).exe:	$(BINDIR) $(PROGRAM).exe
	$(CP) $(PROGRAM).exe $(BINDIR)

clean::		FORCE
	$(RM) -f $(PROGRAM) *.obj *.map *.lst *.bak *.lib *.exe *.i

FORCE:
