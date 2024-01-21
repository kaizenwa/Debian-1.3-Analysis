LEX=flex

EN-SRCS= errlist.m errlist-u.m h-errlist.m siglist.m siglist-u.m \
	getopt.m clnt-misc.m rpc-errlist.m auth.m rpc-misc.m \
	net-misc.m
ENGLISH-SRCS=$(EN-SRCS:%=English/%)

DE-SRCS= auth.m errlist.m getopt.m rpc-errlist.m siglist.m \
	clnt-misc.m h-errlist.m rpc-misc.m errlist-u.m net-misc.m \
	siglist-u.m
GERMAN-SRCS=$(DE-SRCS:%=German/%)

FR-SRCS=$(EN-SRCS)
FRENCH-SRCS=$(FR-SRCS:%=French/%)

NL-SRCS= errlist.m
DUTCH-SRCS=$(NL-SRCS:%=Dutch/%)

TEST-SRCS=$(EN-SRCS)
TESTING-SRCS=$(TEST-SRCS:%=Test/%)

INSTDIR=/etc/locale

LOCALES=C de fr nl

all: C

../gencat: ../gencat.c ../genlib.c
	cd ../; make gencat

../nls_libc.h: $(ENGLISH-SRCS) ../gencat
	../gencat -new /dev/null $(ENGLISH-SRCS) -h $@

C english:  $(ENGLISH-SRCS) ../gencat
	rm -f libc.cat.C
	../gencat -new libc.cat.C $(ENGLISH-SRCS)

de german:   $(GERMAN-SRCS) ../gencat
	rm -f libc.cat.de
	../gencat -new libc.cat.de $(GERMAN-SRCS)

fr french:  $(FRENCH-SRCS) ../gencat
	rm -f libc.cat.fr
	../gencat -new libc.cat.fr $(FRENCH-SRCS)

nl dutch:	$(DUTCH-SRCS) ../gencat
	rm -f libc.cat.nl
	../gencat -new libc.cat.nl $(DUTCH-SRCS)

test:   $(TESTING-SRCS) ../gencat
	rm -f libc.cat.test
	../gencat -new libc.cat.test $(TESTING-SRCS)

check:	$(ENGLISH-SRCS) ../gencat
	@[ -d Check ] || mkdir Check
	@for i in $(EN-SRCS) ;\
	do \
		sed 's/^# /# X-/' English/$$i > Check/$$i ;\
	done
	../gencat -new libc.cat $(ENG-SRCS:%=Check/%)

xtract: xtract.c

xtract.c:	xtract.l
	$(LEX) $<
	mv lex.yy.c $@

clean:
	rm -f libc.cat*

install:
	install -m a+xr -d $(INSTDIR)
	@for i in $(LOCALES); do \
	    if [ -f libc.cat.$$i ]; then \
		install -m a+xr -d $(INSTDIR)/$$i; \
		install -m a+r libc.cat.$$i $(INSTDIR)/$$i/libc.cat; \
		echo installed $(INSTDIR)/$$i/libc.cat; \
	    fi; \
	done
