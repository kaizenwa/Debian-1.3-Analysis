#
# $Id: Makefile,v 3.5 1997/01/01 06:52:03 morioka Exp morioka $
#

EMACS	= mule
FLAGS   = -batch -q -no-site-file -l mk-mel


# Please specify Emacs Lisp install directory:
# MELDIR18 = for Emacs 18.* (NEMACS, NEpoch or MULE 1)
# MELDIR19 = for Emacs 19.* (FSF original, XEmacs or MULE 2)

#MELDIR18 = /usr/local/lib/emacs/local.lisp/tm
#MELDIR19 = /usr/local/lib/mule/site-lisp

MELDIR18 = $(HOME)/lib/emacs18/lisp
MELDIR19 = $(HOME)/lib/emacs19/lisp


RM	= /bin/rm -f
CP	= /bin/cp -p
TAR	= gtar

GOMI	= $(UTILS) *.elc loadpath
FILES	= mel/*.el mel/Makefile mel/mk-mel mel/mel-els \
		tl/emu-*.el mel/ChangeLog

TARFILE = mel-5.6.tar.gz


elc:
	$(EMACS) $(FLAGS) -f compile-mel

install-18:	elc
	$(EMACS) $(FLAGS) -f install-mel $(MELDIR18)

install-19:	elc
	$(EMACS) $(FLAGS) -f install-mel $(MELDIR19)

clean:
	-$(RM) $(GOMI)
	cd ../tl && make clean


tar:
	cd ..; $(TAR) cvzf $(TARFILE) $(FILES)
