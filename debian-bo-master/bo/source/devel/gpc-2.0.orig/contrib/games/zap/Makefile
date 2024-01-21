#
#
# To compile for the sparc/Solaris 2.3:
#   Add -DBSD_COMP to CFLAGS.
#   Add -lucb to LIBES before -ltermcap
#

# Add possible command line flags with this.
# Keep empty in this file.
MYFLAGS=
#
# GPC_VERSION=2.6.3
# SRCDIR=/src/gnu/gpc/$(GPC_VERSION)/contrib/games/zap
SRCDIR=.
LIBES=-ltermcap -lbsd
INCLUDES= -I. -I$(SRCDIR)
CFLAGS= -g $(INCLUDES) $(MYFLAGS)
FUNFLAGS= -Q

all: zap

zap: zap.o screen.o zap-c.o
	gcc $(CFLAGS) -o zap zap.o screen.o zap-c.o $(LIBES) -lgpc -lm

zap.o: zap.pas
	gpc $(CFLAGS) -c zap.pas $(FUNFLAGS)

screen.o: $(SRCDIR)/screen.c
	gcc -c $(CFLAGS) -o $@ $<

zap-c.o: $(SRCDIR)/zap-c.c
	gcc -c $(CFLAGS) -o $@ $<

clean:
	-rm -f zap zap.o screen.o zap-c.o
	-rm -f *~

# Warning: Do not execute the following rules in the source directory.

zap.pas:
	cp $(SRCDIR)/zap.pas zap.pas
