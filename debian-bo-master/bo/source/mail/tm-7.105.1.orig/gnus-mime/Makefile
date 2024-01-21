#
# $Id: Makefile,v 1.1 1996/11/27 18:40:30 morioka Exp morioka $
#

# Please specify emacs executables:
EMACS	= mule
FLAGS   = -batch -q -no-site-file -l g5-path -l Gnus-MIME-MK


PREFIX =


FILES	= gnus-mime/Makefile gnus-mime/*-path gnus-mime/Gnus-MIME-MK \
		gnus-mime/*.el gnus-mime/ChangeLog


TARFILE = gnus-mime-0.07.tar


elc:
	$(EMACS) $(FLAGS) -f compile-gnus-mime

install:
	$(EMACS) $(FLAGS) -f install-gnus-mime $(PREFIX)

clean:
	-rm *.elc


tar:
	cd ..; tar cvf $(TARFILE) $(FILES); gzip -best $(TARFILE)
