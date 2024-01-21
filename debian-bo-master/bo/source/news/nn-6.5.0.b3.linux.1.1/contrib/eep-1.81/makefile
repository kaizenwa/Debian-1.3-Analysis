#
# Makefile for eep .newsrc editor
#
# The following flags are important:
#  UNIX   - compiles UNIX version of code 
#  DIRENT - does your system have dirent.h?
#           See eep.h -- you might need to change path to sys/dirent.h
#  ANSI   - Try to follow ANSI conventions
#

#CC=cc
CC=gcc
 
# On SunOS 4.x systems you will need the following
#LIBS=-lcurses -L/usr/5lib -ltermcap
#HDRS=-I/usr/5include -I/usr/include

# Standard UNIX systems use the following
LIBS=-lcurses
HDRS=
 
# define -DSUBSONLY to not use .newsrc space for unsubscribed groups  -sp
#CCFLAGS=-O -DSUBSONLY -DUNIX -DDIRENT -DANSI $(HDRS)
CCFLAGS=-g -DSUBSONLY -DUNIX -DDIRENT -DANSI $(HDRS)

HDR=eep.h

eep:		eepmain.o eepmisc.o eepmenu.o eepview.o
		$(CC) -o eep eepmain.o eepmisc.o eepmenu.o eepview.o $(LIBS)
		#strip eep

eepmain.o:	eepmain.c $(HDR)
		$(CC) $(CCFLAGS) -c eepmain.c

eepmisc.o:	eepmisc.c $(HDR)
		$(CC) $(CCFLAGS) -c eepmisc.c

eepmenu.o:	eepmenu.c $(HDR)
		$(CC) $(CCFLAGS) -c eepmenu.c

eepview.o:	eepview.c $(HDR)
		$(CC) $(CCFLAGS) -c eepview.c
