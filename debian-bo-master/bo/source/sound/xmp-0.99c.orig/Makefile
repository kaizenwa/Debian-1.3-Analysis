# Extended Module Player makefile
# $Id: Makefile,v 1.7 1997/03/20 13:18:31 claudio Exp $
#
# Edit config.h to configure xmp for your system

VERSION	= 0.99c
DATE	= Thu Mar 20 11:45:42 EST 1997

BIN_DIR	= /usr/local/bin
MAN_DIR = /usr/local/man/man1

CC	= gcc -c -O -Wall -I/usr/lib/oss/include
LD	= gcc -o$@
LIBS	= # -lm

# Use these for AIX 3.2.5 on RS/6000 using the XL C compiler
#CC	= cc -c -qalign=packed -qchars=signed
#LD	= cc -o$@
#LIBS	= # -lm

SHELL	= /bin/sh
OBJS	= xmp.o xm_play.o devices.o misc.o xm_load.o mod_load.o s3m_load.o \
	  stm_load.o 669_load.o far_load.o mtm_load.o ptm_load.o okt_load.o \
	  it_load.o mixer.o
BINS	= xmp
DOCS	= xmp.1
TEST_XM	= high_society.xm
DIST	= xmp-$(VERSION)
DFILES	= README README.bugs README.magic README.formats INSTALL Changes \
	  config.h xmp.c xmp.h xm_play.c misc.c devices.c devices.h \
	  mixer.c load.h dump.c xxm.h xm.h xm_load.c s3m.h s3m_load.c mod.h \
	  mod_load.c stm.h stm_load.c 669.h 669_load.c far.h far_load.c \
	  mtm.h mtm_load.c ptm.h ptm_load.c okt.h okt_load.c it.h it_load.c \
	  FAQ.faq FAQ.txt FAQ.html makefaq #xmp.lsm

.SUFFIXES: .pod .1 .faq .html .txt

.c.o:
	$(CC) $<

.pod.1:
	pod2man --center=" " --release="Version $(VERSION)" $< >$@

.faq.txt:
	makefaq $< >$*.html
	lynx -dump -nolist -underscore $*.html >$@


xmp: $(OBJS) version.o
	$(LD) $(OBJS) version.o $(LIBS)

dump: dump.o
	$(LD) $+

docs: $(DOCS) faq

xmp.1: xmp.pod

test: xmp
	time xmp -vvv $(TEST_XM)

install: installbin installman

installbin: xmp
	@echo Installing binaries in $(BIN_DIR)...
	@install -s -m755 -gbin $(BINS) $(BIN_DIR)

installman: docs
	@echo Installing manpages in $(MAN_DIR)...
	@install -m644 $(DOCS) $(MAN_DIR)
	@echo Done.

clean:
	rm -f *.o $(BINS) core version.c errlist

depend:
	$(CC) -MM -MG $(OBJS:.o=.c) >$@

version.c: $(DFILES) Makefile
	@echo "Creating version id..."; \
	printf "char *ver_id=\"xmp v$(VERSION) $(DATE)\\\n\"\n">$@; \
	printf "\"Copyright (C) 199[67] C Matsuoka, H Carraro Jr\\\n\"\n">>$@; \
	printf "\"AWE-32/awedrv support by Takashi Iwai, Nov 1996\\\n\"\n">>$@; \
	printf "\"Compiled by `whoami`@`uname -n` `date`\";\n" >>$@


# Extra targets:
# 'dist' prepares a distribution package
# 'mark' marks the last RCS revision with the package version number
# 'whatsout' lists the locked files
# 'diff' creates a diff file
# 'faq' generates the FAQ

dist: docs
	rm -f $(DIST).tar.gz
	rm -Rf $(DIST)
	mkdir $(DIST)
	cp -p $(DFILES) xmp.1 xmp.pod $(TEST_XM) $(DIST)
	cat Makefile | \
	sed "s/Thu Mar 20 11:45:42 EST 1997/`date`/" > $(DIST)/Makefile
	mv -f Makefile Makefile.old
	cp $(DIST)/Makefile .
	cd $(DIST); cksum * >FILES; cd ..
	tar cvf - $(DIST) | gzip -c > $(DIST).tar.gz
	rm -Rf $(DIST)
	sync

mark:
	ID=`echo $(VERSION) | sed 's/\./_/g'`; \
	rcs -nxmp_$$ID: RCS/*

whatsout:
	@rlog -R -L RCS/* | sed 's/.*\/\([^\/]*\),v/\1/'

diff:
	@if [ "$(OLDVER)" = "" ]; then \
	    echo "Usage: make diff OLDVER=<old_version>"; \
	else \
	    echo "Creating diff from $(OLDVER) to $(VERSION)"; \
	    diff -rud --new-file xmp-$(OLDVER) xmp-$(VERSION) | gzip -c > \
		xmp-$(OLDVER)-$(VERSION).diff.gz; \
	    sync; \
	fi

faq: FAQ.txt

FAQ.txt: FAQ.html FAQ.faq makefaq

$(OBJS): Makefile

include depend
