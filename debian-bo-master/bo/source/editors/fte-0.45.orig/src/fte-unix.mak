# versions of FTE to build

# Versions:
#  xfte - using XLib (the most stable)
#  vfte - for Linux console directly (with limitations)

#  qfte - with Qt library (in development)
#  mfte - with Motif (in development, very unstable)

#TARGETS = xfte
#TARGETS = xfte qfte
#TARGETS = xfte vfte
#TARGETS = xfte vfte qfte mfte
TARGETS = xfte vfte qfte

PRIMARY = xfte

# choose your os here

#######################################################################
# HP/UX
#UOS      = -DHPUX -D_HPUX_SOURCE
#XINCDIR  = -I/usr/include/X11R5
#XLIBDIR  = -L/usr/lib/X11R5
#MINCDIR  = -I/usr/include/Motif1.2
#MLIBDIR  = -L/usr/lib/Motif1.2

#######################################################################
# Linux
UOS      = -DLINUX
XLIBDIR  = -L/usr/X386/lib

#######################################################################
# AIX
#UOS      = -DAIX -D_BSD_INCLUDES # not recently tested (it did work)

#######################################################################
# Irix
# missing fnmatch, but otherwise ok (tested only on 64bit)
#UOS      = -DIRIX   

#######################################################################
# SunOS (Solaris)
#UOS      = -DSUNOS
#XINCDIR  = -I/usr/openwin/include
#XLIBDIR  = -L/usr/openwin/lib

#######################################################################

#QTDIR   = /users/markom/qt
QLIBDIR  = -L$(QTDIR)/lib
QINCDIR  = -I$(QTDIR)/include

MOC      = moc

LIBDIR   = 
INCDIR   =

#OPTIMIZE = -g
OPTIMIZE = -O2

CC       = g++
LD       = gcc

CCFLAGS  = $(OPTIMIZE) -x c++ -Wall -Wno-unused -DUNIX $(UOS) $(INCDIR) $(XINCDIR) $(QINCDIR) $(MINCDIR)
LDFLAGS  = $(OPTIMIZE) $(LIBDIR) $(XLIBDIR) $(QLIBDIR) $(MLIBDIR)

OEXT     = o

.SUFFIXES: .cpp .o .moc

include objs.inc

XLIBS    = -lX11
QLIBS    = -lqt
VLIBS    = -lgpm
MLIBS    = -lXm -lXt

.cpp.o:
	$(CC) $(CCFLAGS) -c $<

.c.o:
	$(CC) $(CCFLAGS) -c $<

.cpp.moc: 
	$(MOC) $< -o $@

all:    cfte $(TARGETS)
	rm -f fte ; ln -s $(PRIMARY) fte

cfte: cfte.o s_files.o
	$(LD) $(LDFLAGS) cfte.o s_files.o -o cfte 

c_config.o: defcfg.h

defcfg.h: defcfg.cnf
	perl mkdefcfg.pl <defcfg.cnf >defcfg.h

defcfg.cnf: defcfg.fte cfte
	./cfte defcfg.fte defcfg.cnf

xfte: $(OBJS) $(XOBJS)
	$(LD) $(LDFLAGS) $(OBJS) $(XOBJS) $(XLIBS) -o xfte

qfte: g_qt.moc g_qt_dlg.moc $(OBJS) $(QOBJS)
	$(LD) $(LDFLAGS) $(OBJS) $(QOBJS) $(QLIBS) $(XLIBS) -o qfte

vfte: $(OBJS) $(VOBJS)
	$(LD) $(LDFLAGS) $(OBJS) $(VOBJS) $(VLIBS) -o vfte

mfte: $(OBJS) $(MOBJS)
	$(LD) $(LDFLAGS) $(OBJS) $(MOBJS) $(MLIBS) $(XLIBS) -o mfte

g_qt.obj: g_qt.moc

g_qt_dlg.obj: g_qt_dlg.moc

clean:
	rm -f *.o $(TARGETS) defcfg.h defcfg.cnf *.moc
