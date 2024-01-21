# CFINGERD 1.1.1 by Ken Hollis
# Reprogram started 07/13/95
#
# Released under the GPL

CC= gcc
CFLAGS= -g -O2 -Wall -m486
LINK_FLAGS= main.o options.o version.o util.o errors.o strmcpy.o \
	    parse.o configure.o internal.o signal.o standard.o \
	    userlist.o fakeuser.o search.o getutent.o \
	    rfc1413.o wildmat.o log.o

cfingerd:	$(LINK_FLAGS)
		$(CC) -o $@ $^
#		strip cfingerd

clean:
		rm -f *.o cfingerd
		rm -f config.h
