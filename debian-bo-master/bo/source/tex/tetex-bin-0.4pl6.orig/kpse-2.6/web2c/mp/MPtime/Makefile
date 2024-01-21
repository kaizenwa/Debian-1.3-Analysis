##########################################
# Makefile for MetaPost timing tests	 #
##########################################

CC=gcc
OPT=-O

CFLAGS=$(OPT)
LDFLAGS=$(OPT)

#########################################################
# Everything below here should seldom need changing	#
#########################################################

# This target is only for testing arithmetic routines.  Run timemath with
# no arguments and use the results printed on standard output to decide if
# the external routines used when `FIXPT' is not defined in site.h work
# properly and run faster than the standard arithmetic routines.

program = timemath

all: default
default: $(program)

run: timemath
	./timemath

timemath:	time.o cmath.o pmath.o mathtest.o
		$(CC) $(LDFLAGS) -o timemath time.o cmath.o pmath.o mathtest.o

# Note: cmath.c is actually a copy of ../mpmath.c except 
# with `include mpd.h' changed to `include test_mpd.h'.

time.o:     	test_mpd.h mp.h
cmath.o:   	test_mpd.h mp.h
pmath.o:   	test_mpd.h mp.h
mathtest.o: 	test_mpd.h mp.h

mostlyclean:
	rm *.o $(program)

clean: mostlyclean
