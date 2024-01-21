#
# Makefile for manual pages for nntp service
#
# $Id: Makefile,v 1.6 1994/11/17 16:39:52 sob Exp sob $
#

# change these to suit your system
ROFF	= ditroff
PRINTER	= lw301
#MANDIR	= /usr/man

# from here down should be constant
SHELL	= /bin/sh
MACROS	= -man
MANPAGES= nntpd.8c nntpxmit.1 acttimes.8c
EXTRACT	= ./config.sh

all: $(MANPAGES)
	@echo "Done"

nntpd.8c: nntpd.dst $(EXTRACT)
	$(EXTRACT) nntpd.dst nntpd.8c

nntpxmit.1: nntpxmit.dst $(EXTRACT)
	$(EXTRACT) nntpxmit.dst nntpxmit.1

acttimes.8c: acttimes.dst $(EXTRACT)
	$(EXTRACT) acttimes.dst acttimes.8c

install: $(MANPAGES)
	cp nntpd.8c $(MANDIR)/man8/nntpd.8c
	cp acttimes.8c $(MANDIR)/man8/acttimes.8c
	cp nntpxmit.1 $(MANDIR)/man1/nntpxmit.1

print: $(MANPAGES)
	$(ROFF) $(MACROS) -P$(PRINTER) nntpd.8c
	$(ROFF) $(MACROS) -P$(PRINTER) acttimes.8c
	$(ROFF) $(MACROS) -P$(PRINTER) nntpxmit.1

clean:
	-rm -f $(MANPAGES) *.CKP *.BAK *~

distrib: clean
	rm -rf SCCS

check:
	sccs check
