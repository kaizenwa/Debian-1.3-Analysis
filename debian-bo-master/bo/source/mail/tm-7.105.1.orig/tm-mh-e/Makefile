#
# $Id: Makefile,v 6.4 1996/09/04 15:04:03 morioka Exp morioka $
#

# Please specify emacs executables:
#	NEMACS	= for NEMACS (or NEpoch)
#	MULE1	= for Mule 1.* (based on Emacs 18.*)
#	ORIG19	= for Emacs 19.* (FSF original or XEmacs)
#	MULE2	= for MULE 2.* (based on Emacs 19.*)

EMACS	= mule
FLAGS   = -batch -q -no-site-file -l mk-tmh


# Please specify directory to install:
# If it is omitted, shared lisp directory (maybe
# /usr/local/share/emacs/site-lisp or /usr/local/lib/xemacs/site-lisp)
# is used.
DIR =


FILES	= tm/mh-e/*.el tm/mh-e/Makefile tm/mh-e/mk-tmh tm/mh-e/*.ol
TARFILE = tm-mh-e7.63.1.tar


elc:
	$(EMACS) $(FLAGS) -f compile-tm-mh-e

install:
	$(EMACS) $(FLAGS) -f install-tm-mh-e $(DIR)


clean:
	-rm *.elc


tar:
	cd ../..; tar cvf $(TARFILE) $(FILES); gzip -9 $(TARFILE)
