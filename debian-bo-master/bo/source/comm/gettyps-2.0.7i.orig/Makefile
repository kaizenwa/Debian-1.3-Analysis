# Makefile for getty 2.0.7 distribution
#

#
# FSSTND Compliance:
#   If you want getty_ps to comply with the upcoming FSSTND document,
#   uncomment the following line.  As a result, all config files will
#   be expected in /etc/conf.* rather than /etc/default, modem locks 
#   will be created in /var/lock, and binaries will go in /sbin.
# 
# FSSTND=-DFSSTND

SHELL=	/bin/sh
CC=	gcc

#CFLAGS=	-g -fpcc-struct-return ${FSSTND}
#LFLAGS=	-g
CFLAGS=	-O3 -fomit-frame-pointer -fpcc-struct-return ${FSSTND}
LFLAGS=	-s

LIBS=	-ltermcap
LLIBS=	

# where things go
BIN=    /sbin
UUBIN=  /sbin

# what are we making
SRC=	main.c funcs.c defaults.c table.c utmp2.c
OBJ=	main.o funcs.o defaults.o table.o utmp2.o
UUSRC=	uufuncs.c main.c funcs.c defaults.c table.c utmp2.c
UUOBJ=	uufuncs.o uumain.o funcs.o defaults.o table.o utmp2.o

# rules

all:	getty uugetty

getty:	$(OBJ)
	$(CC) $(LFLAGS) -o $@ $(OBJ) $(LIBS) 

uugetty: $(UUOBJ)
	$(CC) $(LFLAGS) -o $@ $(UUOBJ) $(LIBS) 

test: getty
	-mv $(BIN)/testgetty $(BIN)/testgetty-
	mv getty $(BIN)/testgetty

install:
	-mv $(BIN)/getty $(BIN)/getty-
	-mv $(UUBIN)/uugetty $(UUBIN)/uugetty-
	-cp getty $(BIN)
	-cp uugetty $(UUBIN)
	chmod 700 $(BIN)/getty $(UUBIN)/uugetty

clean:
	rm -f *.o core *.out *~ .depend Make.Log Makefile.old

clobber: clean
	rm -f getty uugetty

realclean: clobber

depend:	
	gcc -M *.c > .depend

uumain.o:	main.c
	$(CC) $(CFLAGS) -DUUGETTY -c main.c -o uumain.o


# include dependency file
ifeq (.depend,$(wildcard .depend))
include .depend
endif
