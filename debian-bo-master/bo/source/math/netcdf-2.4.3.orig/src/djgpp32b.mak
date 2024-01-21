#   Copyright 1996 University Corporation for Atmospheric Research
#
#	PC Makefile for common makefile rules used by the netCDF
#	package.

BISON		= bison
FLEX		= flex

F2C		= f2c
F2CFLAGS	= -w66 -A -g

CC		= gcc
CFLAGS		=
CPPFLAGS	= -DMSDOS $(DEFINES) $(INCLUDES)

CXX		= gcc -x c++
CXXFLAGS	=

F77		=
FFLAGS		=

AR		= ar
ARFLAGS		= -rcus

CAT		= cat
CP		= copy
RM		= rm
MKDIR		= mkdir
# `echo N | comp' is an alternative to the following:
DIFF		= diff

LDFLAGS		= 

.SUFFIXES:
.SUFFIXES:	.exe .o .c .f .cc

# Translate FORTRAN source code into C source code:
.f.c:
	$(F2C) $(F2CFLAGS) $< >$@

# Compile FORTRAN source code:
.f.o:
	$(F2C) $(F2CFLAGS) $<
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $*.c
	$(RM) $*.c

# Compile C source code:
.c.o:
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $<

# Compile C++ source code:
.cc.o:
	$(CXX) -c $(CXXFLAGS) $(CPPFLAGS) $<

# Link an object into an executable:
.o.exe:
	$(CC) -o $* $(LDFLAGS) $< $(OBJS) $(LIBS)
	coff2exe $*
	$(RM) $*

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
	$(CC) -o $(PROGRAM) $(LDFLAGS) $(OBJS) $(LIBS)
	coff2exe $(PROGRAM)
	$(RM) $(PROGRAM)

$(BINDIR)/$(PROGRAM).exe:	$(BINDIR) $(PROGRAM).exe
	$(CP) $(PROGRAM).exe $(BINDIR)

clean::		FORCE
	$(RM) -f $(PROGRAM) *.o *.map *.lst *.bak *.a *.exe

FORCE:
