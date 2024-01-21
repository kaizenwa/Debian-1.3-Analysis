#
# this makefile for cdtools, copyright 1994 thomas insel
#

# where do we install the binaries and unformatted manual pages?
prefix = /usr
bindir = $(prefix)/bin
sbindir = $(prefix)/sbin
mandir = $(prefix)/man

# SRCS are RCS'd files
SRCS = main.c commands.c hardware.c database.c info.c cdtool.1

# OTHERS are other files that are required in a distribution package
OTHERS = Makefile cdadd.sh README COPYING config.h database.h hardware.h

# OBJS are used to build cdtool
OBJS = main.o commands.o hardware.o database.o info.o

# LINKS are different names that cdtool can be called by
LINKS = cdplay cdpause cdstop cdeject cdir cdvolume

# We seem to need GCC
CC = gcc

all: cdtool links

dist: $(SRCS) $(OTHERS)
	rm -rf ../cdtool-xx
	mkdir ../cdtool-xx
	cp $(SRCS) $(OTHERS) ../cdtool-xx

cdtool: $(OBJS)
	$(CC) -o cdtool $(OBJS)

links:
	for i in $(LINKS); do \
	    if test -h $$i; then true; else rm -f $$i; ln -s cdtool $$i; fi; \
	done

install: cdtool cdtool.1
	install -d $(bindir) -o root -m 0755
	install cdtool $(bindir) -s -o root
	install cdadd.sh $(bindir)/cdadd -o root
	install -d $(sbindir) -o root -m 0755
	install changeaudiocddev $(sbindir)/changeaudiocddev -o root
	install -d $(mandir)/man1 -o root -m 0755
	install -m 0644 cdtool.1 $(mandir)/man1
	cd $(bindir) ; \
	for i in $(LINKS); do \
		ln -s cdtool $$i; \
	done

	cd $(mandir)/man1 ; \
	for i in $(LINKS); do \
		ln -s cdtool.1 $$i.1; \
	done

uninstall: dummy
	rm -f $(mandir)/man1/cdtool.1 $(bindir)/cdtool $(bindir)/cdadd
	for i in $(LINKS); do \
		rm -f $(bindir)/$$i $(mandir)/man1/$$i.1; \
    done

dummy:

distclean: clean

clean:
	rm -rf *.o $(LINKS) cdtool

realclean: clean
	for i in $(SRCS); do \
	    if test -w $$i; then ci $$i; fi; \
	    if test -e $$i; then rm -f $$i; fi; \
	done

.DEFAULT:
	co $<

cdtool.1:
	co cdtool.1

# dependencies 
main.o: config.h
database.o: database.h
hardware.o: database.h hardware.h
info.o: database.h hardware.h
main.o: config.h
