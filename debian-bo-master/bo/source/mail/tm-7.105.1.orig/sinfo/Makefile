#
# $Id: Makefile,v 1.1 1996/12/04 05:03:04 morioka Exp morioka $
#

EMACS	= emacs
FLAGS   = -batch -q -no-site-file -l SINFO-MK

PREFIX =

FILES	= sinfo/Makefile sinfo/SINFO-* \
		sinfo/*.el \
		tl/*.el tl/TL-ELS	\
		emu/*.el emu/EMU-ELS

TARFILE = sinfo-0.1.tar


elc:
	$(EMACS) $(FLAGS) -f compile-sinfo

install:	elc
	$(EMACS) $(FLAGS) -f install-sinfo $(PREFIX)


clean:
	-rm *.elc


tar:
	cd ..; tar cvf $(TARFILE) $(FILES); gzip -best $(TARFILE)
