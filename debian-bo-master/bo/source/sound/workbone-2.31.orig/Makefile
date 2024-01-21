# Linux Makefile for WorkBone 2.3 Audio CD Control Panel
# 
export CFLAGS
.PHONY	: all clean install lazy
srcdir	:= .
CC	:= gcc
CFLAGS	:= -O2 -fomit-frame-pointer $(machine)
LDFLAGS	:= -s
WARN	:= -Wall -Wmissing-prototypes 
#WARN	:= -Wall -Wmissing-prototypes -Wshadow -Wcast-qual -Wcast-align -Wwrite-strings -Wconversion \
#       -Wstrict-prototypes -Wnested-externs -Wformat
bindir	:= /usr/local/bin
mandir	:= /usr/man/man1
mangrp	:= daemon
manown	:= daemon

##kernver := $(basename $(shell uname -r))
##ifeq ($(kernver),1.0)
##CFLAGS := $(CFLAGS) -DKVER_10
##endif

.c.o:
	$(CC) $(WARN) $(CFLAGS) -c $*.c

all: workbone
	
lazy:
	$(MAKE) all CFLAGS:='$(CFLAGS) -DWBLAZY'

workbone: workbone.o hardware.o  
	$(CC) -o $@ $^ $(LDFLAGS)

workbone.1.Z: workbone.1
	groff -man $< | compress > $@
		
workbone.doc: workbone.1
	groff -man $< | col -bx > $@

hardware.o: hardware.c struct.h

workbone.o: workbone.c workbone.h

install: workbone
	install -c -m755 $(binown) $(srcdir)/workbone $(bindir)
	install -c -m644 -o $(manown) -g $(mangrp) $(srcdir)/workbone.1 $(mandir)

clean:
	rm -f workbone workbone.doc workbone.1.Z *.o core *~ erro

