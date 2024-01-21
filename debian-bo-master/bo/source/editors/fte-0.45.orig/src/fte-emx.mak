INCDIR    =
LIBDIR    =

#OPTIMIZE  = -g -DDEBUG_EDITOR
OPTIMIZE  = -O2 -s -DDEBUG_EDITOR

MT        = -Zmt

CC        = gcc
LD        = gcc

#XTYPE      = -Zomf
#XLTYPE     = -Zomf -Zsys -Zlinker /map -Zlinker /runfromvdm
#OEXT=obj

OEXT=o


CCFLAGS   = $(OPTIMIZE) -pipe $(MT) $(XTYPE) -x c++ -Wall -Wno-unused -DOS2 -DEMX $(INCDIR)
LDFLAGS   = $(OPTIMIZE) $(MT) -Zmap $(XLTYPE) $(LIBDIR)

.SUFFIXES: .cpp .$(OEXT)

include objs.inc

.cpp.$(OEXT):
	$(CC) $(CCFLAGS) -c $<

.c.$(OEXT):
	$(CC) $(CCFLAGS) -c $<

all: cfte.exe fte.exe ftepm.exe clipserv.exe cliputil.exe

clipserv.exe: clipserv.$(OEXT) clipserv.def
	$(LD) $(LDFLAGS) clipserv.$(OEXT) clipserv.def -o clipserv.exe

cliputil.exe: cliputil.$(OEXT) clip_vio.$(OEXT) cliputil.def
	$(LD) $(LDFLAGS) cliputil.$(OEXT) clip_vio.$(OEXT) cliputil.def -o cliputil.exe

cfte.exe: $(CFTE_OBJS) cfte.def
	$(LD) $(LDFLAGS) $(CFTE_OBJS) cfte.def -o cfte.exe

defcfg.cnf: defcfg.fte cfte.exe
	cfte defcfg.fte defcfg.cnf

defcfg.h: defcfg.cnf bin2c.exe
	bin2c defcfg.cnf >defcfg.h

bin2c.exe: bin2c.cpp
	$(CC) $(CCFLAGS) bin2c.cpp -o bin2c.exe

c_config.$(OEXT): defcfg.h

fte.exe: $(OBJS) $(VIOOBJS) fte.def
	$(LD) $(LDFLAGS) $(OBJS) $(VIOOBJS) fte.def -o fte.exe

ftepm.res: ftepm.rc pmdlg.rc
	rc -r -i \emx\include ftepm.rc ftepm.res

ftepm.exe: $(OBJS) $(PMOBJS) ftepm.def ftepm.res
	$(LD) $(LDFLAGS) $(OBJS) $(PMOBJS) ftepm.def ftepm.res -o ftepm.exe


#rc -i \emx\include ftepm.rc ftepm.exe

#ftepm.exe:: ftepm.res
#	rc ftepm.res ftepm.exe

