#  Makefile for TIGER mapping programs

SHELL=/bin/sh

ICONT = icont
IFLAGS = -us
ITRAN = $(ICONT) $(IFLAGS)

DEST=/unspecified/destination


PROGS = tgrprep tgrlink tgrmap tgrquant
SCRIPTS = tgrsort tgrstats


default:	$(PROGS)

tgrprep:	tgrprep.icn	; $(ITRAN) tgrprep
tgrlink:	tgrlink.icn	; $(ITRAN) tgrlink
tgrmap:		tgrmap.icn	; $(ITRAN) tgrmap
tgrquant:	tgrquant.icn	; $(ITRAN) tgrquant

install:	$(PROGS) $(SCRIPTS)
		cp $(PROGS) $(SCRIPTS) $(DEST)

clean Clean:			; rm -f $(PROGS) *.u[12]
