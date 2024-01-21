
CXXFLAGS = $(BASIC_C_FLAGS) -I.. 
CFLAGS = $(CXXFLAGS)

SRCS = error.cc ghost.cc movement.cc lux.cc dotmap.cc attack.cc \
	agemap.cc sigs.cc search.cc bigdotmap.cc run.cc globals.cc \
	runnable.cc label.cc runlist.cc fruit.cc score.cc banner.cc \
	calib.cc highscore.cc play.cc sound.cc sndclient.c lf.cc isleep.cc \
	sf.cc vts.cc maze.cc

OBJS = error.o ghost.o movement.o lux.o dotmap.o attack.o \
	agemap.o sigs.o search.o bigdotmap.o run.o globals.o \
	runnable.o label.o runlist.o fruit.o score.o banner.o \
	calib.o highscore.o play.o sound.o sndclient.o lf.o isleep.o \
	sf.o vts.o maze.o

SRCX = map.cc sndserver.c luxchk.cc

ifeq ($(strip $(TOP_LEVEL)),)
all: err_msg
else
ifeq (.depend,$(wildcard .depend))
all: ../luxman-snd ../luxchk ../luxman  
include .depend
else
all: dep ../luxman-snd ../luxchk ../luxman 
endif
endif

LIBNAMES = ../lib/libmisc.a ../rawkey/librawkey.a ../gtools/libgtools.a \
	../argv/libargv.a
LIBS = $(LIBNAMES) -lvga -lm

LIBPATH =

err_msg:
	@echo '+-------------------------------------------+'
	@echo '| Please run the top level Makefile instead |'
	@echo '|            of this one                    |'
	@echo '+-------------------------------------------+'

../luxman-snd:	sndserver.c
	gcc $(LDFLAGS) $(CFLAGS) -o ../luxman-snd sndserver.c

../luxchk: $(OBJS) luxchk.o $(LIBNAMES)
	g++ $(LDFLAGS) $(LIBPATH) -o ../luxchk luxchk.o $(OBJS) $(LIBS)

../luxman: $(OBJS) map.o $(LIBNAMES)
	g++ $(LDFLAGS) $(LIBPATH) -o ../luxman map.o $(OBJS) $(LIBS)
	chmod +s ../luxman

clean:
	rm -f ../luxman ../luxchk ../luxman-snd *.o *.*~ *~ .depend core

dep:
	rm -f .depend
	gcc -c $(CXXFLAGS) -M $(SRCS) $(SRCX) > .depend

