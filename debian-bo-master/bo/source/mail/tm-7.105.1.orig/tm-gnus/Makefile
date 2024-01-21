#
# $Id: Makefile,v 7.10 1996/09/04 15:18:08 morioka Exp morioka $
#

# Please specify emacs executables:
EMACS	= mule
FLAGS   = -batch -q -no-site-file -l mk-tgnus


# Please specify directory to install:
# If it is omitted, shared lisp directory (maybe
# /usr/local/share/emacs/site-lisp or /usr/local/lib/xemacs/site-lisp)
# is used.
DIR =


FILES	= tm-gnus/Makefile tm-gnus/*-path tm-gnus/mk-tgnus \
		tm-gnus/*.el tm-gnus/ChangeLog tm/doc/tm-gnus*.texi 


TARFILE = tm-gnus7.72.tar


gnus:
	$(EMACS) $(FLAGS) -f compile-tm-gnus

gnus3:
	$(EMACS) $(FLAGS) -l g3-path -f compile-tm-gnus

gnus4:
	$(EMACS) $(FLAGS) -l g4-path -f compile-tm-gnus

install:
	$(EMACS) $(FLAGS) -f install-tm-gnus $(DIR)

clean:
	-rm *.elc


tar:
	cd ../..; tar cvf $(TARFILE) $(FILES); gzip -9 $(TARFILE)
