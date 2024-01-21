#-----------------------------------------------------------------------------
# Makefile for xvboxled
#-----------------------------------------------------------------------------

# this is where make install will install the executable

BINDIR = /usr/local/bin

# and this is the path to the manpages

MANDIR = /usr/local/man

# If you use a different compiler or different compiler settings, change
# these lines accordingly
CC = gcc
CCFLAGS = -O2 -Wall
LDFLAGS = -lX11 -L/usr/X11R6/lib
RM = rm -f

#-----------------------------------------------------------------------------
# You will probably not want to edit anything below this line
#-----------------------------------------------------------------------------
VERSION = 1.1
DEFS = -DNO_COLOR_ERRORS -DVERSION="\"$(VERSION)\""

PROG = xvboxled
OBJS = xvboxled.o xinit.o windows.o colors.o
SRCS = xvboxled.c xinit.c windows.c colors.c


$(PROG) : $(OBJS) 
	$(CC) $(CCFLAGS) -o $(PROG) $(OBJS) $(LDFLAGS)

xvboxled.o : xvboxled.c xinit.h global.h windows.h colors.h config.h
	$(CC) $(CCFLAGS) $(DEFS) -c xvboxled.c -o xvboxled.o 

xinit.o : xinit.c global.h 
	$(CC) $(CCFLAGS) $(DEFS) -c xinit.c -o xinit.o 

windows.o : windows.c windows.h global.h   
	$(CC) $(CCFLAGS) $(DEFS) -c windows.c -o windows.o

colors.o : colors.c colors.h global.h 
	$(CC) $(CCFLAGS) $(DEFS) -c colors.c -o colors.o


install: xvboxled
	mkdir -p $(BINDIR)
	install -c xvboxled $(BINDIR)
	install -c xvboxled.1x.gz $(MANDIR)/man1


clean :
	$(RM) $(PROG) $(OBJS) *~



















