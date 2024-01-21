#
# $Id: Makefile,v 7.45 1997/01/10 15:42:29 morioka Exp morioka $
#

VERSION = 7.105.1

SHELL	= /bin/sh
MAKE	= make
CC	= gcc
CFLAGS	= -O2
RM	= /bin/rm -f
CP	= /bin/cp -p
EMACS	= emacs

BINS	= src/ol2 src/decode-b
UTILS	= $(BINS)
GOMI	= $(BINS) mime-setup.el mime-setup.el~ \#mime-setup.el\# *.elc \
		.gnus-compile*
FLAGS   = -batch -q -no-site-file

PREFIX	= NONE
EXEC_PREFIX =

TM_FILES = tm/README.en tm/ChangeLog \
		tm/Makefile tm/TM-MK tm/TM-ELS tm/TM-CFG \
		tm/tm-def.el \
		tm/tm-eword.el tm/tm-ew-d.el tm/tm-ew-e.el \
		tm/tm-view.el tm/tm-parse.el \
		tm/tm-text.el tm/tm-image.el \
		tm/tm-play.el tm/tm-partial.el tm/tm-pgp.el \
		tm/tm-ftp.el tm/tm-latex.el tm/tm-html.el tm/tm-file.el \
		tm/tm-tar.el \
		tm/tm-bbdb.el \
		tm/tm-edit*.el tm/signature.el \
		tm/tm-setup.el tm/mime-setup.el.in \
		tm/sc-setup.el \
		tm/methods/tm* tm/methods/ChangeLog \
		tm/src/*.c \
		tm/doc/*.sgml tm/doc/*.texi tm/doc/*.info \
		tm/doc/Makefile tm/doc/*.ol tm/doc/*.tex \
		tm/old-logs/*.en tm/old-logs/*.ja

TM_MH_E_FILES =	tm-mh-e/Makefile tm-mh-e/mk-tmh tm-mh-e/TMH-ELS \
		tm-mh-e/*.el tm-mh-e/ChangeLog \
		tm-mh-e/*.sgml tm-mh-e/*.texi tm-mh-e/*.info

GNUS_MIME_FILES = gnus-mime/Makefile gnus-mime/*-path \
		gnus-mime/Gnus-MIME-* gnus-mime/*.el \
		gnus-mime/*.sgml gnus-mime/*.texi gnus-mime/*.info \
		gnus-mime/ChangeLog

TM_GNUS_FILES =	tm-gnus/Makefile tm-gnus/*-path tm-gnus/mk-tgnus \
		tm-gnus/TGNUS-ELS tm-gnus/*.el tm-gnus/ChangeLog \
		tm-gnus/*.ol tm-gnus/*.texi tm-gnus/*.info tm-gnus/*.tex

TM_MAIL_FILES = tm-mail/TMAIL-ELS tm-mail/*.el tm-mail/ChangeLog

TM_VM_FILES =	tm-vm/TM-VM-ELS tm-vm/*.el tm-vm/*.texi tm-vm/*.info \
		tm-vm/ChangeLog

TM_MUA_FILES =	$(TM_MH_E_FILES) $(GNUS_MIME_FILES) $(TM_GNUS_FILES) \
		$(TM_MAIL_FILES) $(TM_VM_FILES)

MEL_FILES = mel/*.el mel/Makefile mel/mk-mel mel/MEL-ELS mel/ChangeLog

MU_FILES =	mu/MU-ELS mu/*.el mu/ChangeLog

TL_FILES =	tl/README.en tl/Makefile tl/mk-tl tl/TL-ELS \
		tl/*.el tl/ChangeLog

BITMAP_FILES =	bitmap-mule/BITMAP-* bitmap-mule/Makefile \
		bitmap-mule/*.el bitmap-mule/*.bdf \
		bitmap-mule/README.* bitmap-mule/ChangeLog

EMU_FILES =	emu/EMU-ELS emu/*.el emu/ChangeLog

SINFO_FILES =	sinfo/SINFO-* sinfo/Makefile \
		sinfo/*.dtd sinfo/*.el sinfo/*-mapping sinfo/ChangeLog

FILES	= $(TM_FILES) $(TM_MUA_FILES) $(MEL_FILES) $(MU_FILES) \
		$(TL_FILES) $(BITMAP_FILES) $(EMU_FILES) $(SINFO_FILES)

elc:
	$(EMACS) $(FLAGS) -l TM-MK -f compile-tm $(PREFIX) $(EXEC_PREFIX)
#	$(SHELL) .gnus-compile

install-elc:	elc
	$(EMACS) $(FLAGS) -l TM-MK -f install-tm $(PREFIX) $(EXEC_PREFIX)


all:	$(UTILS) $(DVI) elc

src/ol2:	src/ol2.c
	$(CC) $(CFLAGS) src/ol2.c -o src/ol2

src/decode-b:	src/decode-b.c
	$(CC) $(CFLAGS) src/decode-b.c -o src/decode-b


tex:	ol2
	cd doc; $(MAKE) tex

dvi:	ol2
	cd doc; $(MAKE) dvi

ps:	ol2
	cd doc; $(MAKE) ps


install:	$(UTILS) methods install-elc install-execs

execs:	$(UTILS)

install-execs:
	$(EMACS) $(FLAGS) -l TM-MK -f install-execs $(PREFIX) $(EXEC_PREFIX)


update-xemacs:
	$(EMACS) $(FLAGS) -l TM-MK -f update-xemacs-source


clean:
	-$(RM) $(GOMI)
	-cd doc   && $(MAKE) clean
	-cd gnus  && $(MAKE) clean
	-cd mh-e  && $(MAKE) clean
	cd ../mel && $(MAKE) clean


oomori:
	cd doc; $(MAKE) tex
	cd ..; gtar cvf tm-$(VERSION).tar $(FILES)
	-cd ..; mkdir tm-$(VERSION)
	cp ../tm-oomori/README.?? ../tm-$(VERSION)
	cd ../tm-$(VERSION); gtar xvf ../tm-$(VERSION).tar
	cd ..; gtar cvzf tm-$(VERSION).tar.gz tm-$(VERSION)
	cd ..; $(RM) -r tm-$(VERSION); rm tm-$(VERSION).tar


release:
	cd ..; mv tm$(VERSION).tar.gz /pub/GNU/elisp/mime/beta/
	-cd /pub/GNU/elisp/mime/beta/ ; rm tm-oomori-current.tar.gz
	cd /pub/GNU/elisp/mime/beta/ ; ln -s tm$(VERSION).tar.gz tm-oomori-current.tar.gz
