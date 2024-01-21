#
# fwtk/doc/man/Makefile 1996-08-08
# Michael Shields <shields@crosslink.net>
#

include	../../Makefile.config

all:

clean:

install:	all
	for f in *.3; do \
	  install -g root -m 644 -o root $$f $(DESTMAN)/man3/$$f; done
	for f in *.5; do \
	  install -g root -m 644 -o root $$f $(DESTMAN)/man5/$$f; done
	for f in *.8; do \
	  install -g root -m 644 -o root $$f $(DESTMAN)/man8/$$f; done
