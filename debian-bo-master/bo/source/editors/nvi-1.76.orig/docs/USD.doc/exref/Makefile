# @(#)Makefile	8.7 (Berkeley) 8/18/96

ROFF=	groff
TBL=	tbl

all: exref.ps summary.ps

exref.ps: ex.rm
	${TBL} ex.rm | ${ROFF} -ms > $@
	chmod 444 $@

summary.ps: ex.summary
	${TBL} ex.summary | ${ROFF} > $@
	chmod 444 $@

clean:
	rm -f exref.ps summary.ps
