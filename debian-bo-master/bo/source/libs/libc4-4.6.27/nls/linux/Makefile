LEX=flex

ENG-SRCS= errlist.m errlist-u.m h-errlist.m siglist.m siglist-u.m \
	getopt.m clnt-misc.m rpc-errlist.m auth.m rpc-misc.m \
	net-misc.m
ENGLISH-SRCS=$(ENG-SRCS:%=English/%)

GER-SRCS= auth.m errlist.m getopt.m rpc-errlist.m siglist.m \
	clnt-misc.m h-errlist.m rpc-misc.m errlist-u.m net-misc.m \
	siglist-u.m
GERMAN-SRCS=$(GER-SRCS:%=German/%)

FR-SRCS=$(ENG-SRCS)
FRENCH-SRCS=$(ENG-SRCS:%=French/%)

TEST-SRCS=$(ENG-SRCS)
TESTING-SRCS=$(TEST-SRCS:%=Test/%)

INSTDIR=/etc/locale/C

libc.cat: all

all: english

../gencat: ../gencat.c ../genlib.c
	cd ../; make gencat

../nls_libc.h: $(ENGLISH-SRCS) ../gencat
	../gencat -new /dev/null $(ENGLISH-SRCS) -h $@

english:  $(ENGLISH-SRCS) ../gencat
	rm -f libc.cat
	../gencat -new libc.cat $(ENGLISH-SRCS)

german:   $(GERMAN-SRCS) ../gencat
	../gencat -new libc.cat $(GERMAN-SRCS)

french:  $(FRENCH-SRCS) ../gencat
	rm -f libc.cat
	../gencat -new libc.cat $(FRENCH-SRCS)

test:   $(TESTING-SRCS) ../gencat
	../gencat -new libc.cat $(TESTING-SRCS)

check:	$(ENGLISH-SRCS) ../gencat
	@[ -d Check ] || mkdir Check
	@for i in $(ENG-SRCS) ;\
	do \
		sed 's/^# /# X-/' English/$$i > Check/$$i ;\
	done
	../gencat -new libc.cat $(ENG-SRCS:%=Check/%)

xtract: xtract.c

xtract.c:	xtract.l
	$(LEX) $<
	mv lex.yy.c $@
	
install: libc.cat
	install -m a+xr -d $(INSTDIR)
	install -m a+r libc.cat $(INSTDIR)
