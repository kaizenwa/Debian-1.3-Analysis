# =()<@<INCLUDE>@>()=
.include "../include/config.make"

BINS=gngp newslock ctime canonhdr getabsdate sizeof dbz\
	now $(DOSPACEFOR)
PROGS=$(BINS) newshostname report lock unlock touchlock staleness dbzinfo \
	spacefor queuelen mkpdir
DEST=$(NEWSBIN)
ALLNB=$(PROGS) namecheck.awk canonsys.awk
ALL=$(ALLNB)

all:	$(ALL)
	$(MX) $(PROGS)

install:	$(ALL)
	$(MKDIR) $(DEST)
	$(TO) $(DEST) $(ALLNB)

setup:	$(ALL)

cmp:	$(ALL)
	@$(IN) $(DEST) $(ALLNB)
	: the numbers here do not matter, but all the spacefors should succeed
	$(DEST)/spacefor 1000 incoming
	$(DEST)/spacefor 1000 articles
	$(DEST)/spacefor 1000 control
	$(DEST)/spacefor 1000 outbound
	$(DEST)/spacefor 1000 batchfiles
	$(DEST)/spacefor 1000 archive

$(BINS):	$(LIB)

gngp:	gngp.o
	$(CC) $(LDFLAGS) gngp.o $(LIBS) -o $@

ctime:	ctime.o
	$(CC) $(LDFLAGS) ctime.o $(LIBS) -o $@

newslock:	newslock.o
	$(CC) $(LDFLAGS) newslock.o $(LIBS) -o $@

canonhdr:	canonhdr.o
	$(CC) $(LDFLAGS) canonhdr.o $(LIBS) -o $@

getabsdate:	getabsdate.o
	$(CC) $(LDFLAGS) getabsdate.o $(LIBS) -o $@

sizeof:	sizeof.o
	$(CC) $(LDFLAGS) sizeof.o $(LIBS) -o $@

dbz:	dbz.o
	$(CC) $(LDFLAGS) dbz.o $(LIBS) -o $@

dbz.c:	../libdbz/dbzmain.c
	cp ../libdbz/dbzmain.c $@

now:	now.o
	$(CC) $(LDFLAGS) now.o $(LIBS) -o $@

spacefor:	sf.$(SPACEFOR)
	cp sf.$(SPACEFOR) $@

queuelen:	queuelen.$(QUEUELEN)
	cp queuelen.$(QUEUELEN) $@

sf.bsd:	sfshproto makefile
	cp sfshproto $@

sf.v7:	sfshproto makefile
	sed '/dfunit=/s/1024/512/;/nr = 2/s//nr = 1/;/nf = 4/s//nf = 2/;/stupid/d' sfshproto >$@

sf.sysv:	sfshproto makefile
	sed '/dfunit=/s/1024/512/;/awk/s~|~| sed "s/.*:/: :/" |~;/nf = 4/s//nf = 3/;/stupid/s/4BSD/System V/' sfshproto >$@

sf.xenix:	sfshproto makefile
	sed '/dfunit=/s/1024/512/;/awk/s~|~| sed "s/.*:/: :/" |~;/nr = 2/s//nr = 1/;/nf = 4/s//nf = 3/;/stupid/s/4BSD/Xenix/' sfshproto >$@

sf.sgi:	sfshproto makefile
	sed '/dfunit=/s/1024/512/;/nf = 4/s//nf = 5/;/stupid/s/4BSD/SGI/' sfshproto >$@

sf.null:	makefile
	echo '#! /bin/sh' >$@
	echo 'echo 100' >>$@

sf.statfs sf.statvfs sf.ustat sf.ultrix:	sfcproto makefile dospacefor
	cp sfcproto sf.$(SPACEFOR)

dospacefor:	dowhatever.o do$(SPACEFOR).o
	$(CC) $(LDFLAGS) dowhatever.o do$(SPACEFOR).o $(LIBS) -o $@

queuelen.pre:	queuelen.vo
	sed '/grade=d/s//grade=./' queuelen.vo >$@

queuelen.null:
	echo '#! /bin/sh' >$@
	echo 'echo 0' >>$@

r:	$(ALL) rclean
	$(MX) $(PROGS) shellcheck
	: If this fails, you missed a vital path directory, and later
	: regression tests would fail messily for that reason.
	$(HERE) test " `echo hi | wc -l`" -eq 1 ;
	: If this fails, you have a buggy shell, probably bash, and the
	: C News locking system will not work properly as a result.
	./shellcheck
	: a sketchy test of locking
	$(HERE) ./lock LOCKtest $$$$
	test -s LOCKtest ;
	: If the following fails, you probably have either a buggy make
	: or a buggy shell, e.g. an old version of bash.
	$(HERE) if ./lock -o LOCKtest $$$$ ; then false ; else true ; fi
	test -s LOCKtest ;
	: test that touchlock really does update locks
	echo hi >tmp.lock
	sleep 2
	$(HERE) ./touchlock LOCKtest
	test -s LOCKtest ;
	test " `find L* -newer tmp.lock -print`" = " LOCKtest" ;
	: and try unlocking things
	$(HERE) ./unlock LOCKtest
	test ! -f LOCKtest ;
	: ctime
	: 756362361 is Mon, 20 Dec 1993 04:39:21 GMT -- checked by calculator
	test "`./ctime -u 756362361`" = "Mon Dec 20 04:39:21 1993"
	test "`./ctime -i 756362361`" = "Mon, 20 Dec 1993 04:39:21 GMT"
	: getabsdate
	test "`./getabsdate 'Mon Dec 20 04:39:21 GMT 1993'`" = "756362361"
	test "`./getabsdate 'Mon, 20 Dec 1993 04:39:21 GMT'`" = "756362361"
	test "`./getabsdate 'Sun, 19 Dec 1993 23:39:21 -0500'`" = "756362361"
	test "`./getabsdate \"\`./ctime -i 756362361\`\"`" = "756362361"
	NEWSTZ=3660 ; export NEWSTZ ; test "`./getabsdate 'Dec 20 03:38:21 1993'`" = "756362361"
	: canonhdr
	echo 'newsgroups: sci.space' >tmp.in
	echo 'Newsgroups: sci.space' >tmp.good
	echo 'MeSsage-id: <nothing>' >>tmp.in
	echo 'Message-ID: <nothing>' >>tmp.good
	echo 'PATH: nowhere!nobody' >>tmp.in
	echo 'Path: nowhere!nobody' >>tmp.good
	echo 'folloWup-To: poster' >>tmp.in
	echo 'Followup-To: poster' >>tmp.good
	cp tmp.good tmp.gooddate
	echo 'dATe: Sat, 18 Dec 1993 23:39:21 -0500' >>tmp.in
	echo 'Date: Sat, 18 Dec 1993 23:39:21 -0500' >>tmp.good
	echo 'Date: Sun, 19 Dec 1993 04:39:21 GMT' >>tmp.gooddate
	echo 'Expires: Sun, 19 Dec 1993 23:39:21 -0500' >>tmp.in
	echo 'Expires: Sun, 19 Dec 1993 23:39:21 -0500' >>tmp.good
	echo 'Expires: Mon, 20 Dec 1993 04:39:21 GMT' >>tmp.gooddate
	echo >>tmp.in
	echo 'Deleted: gone' >>tmp.in
	./canonhdr <tmp.in >tmp.out
	cmp tmp.good tmp.out || diff tmp.good tmp.out
	./canonhdr -d <tmp.in >tmp.out
	cmp tmp.gooddate tmp.out || diff tmp.gooddate tmp.out
	sed 's/, 19 Dec 1993.*/ Dec 19 23:39:21 EST 1993/' tmp.in >tmp.din
	./canonhdr -d <tmp.din >tmp.out
	cmp tmp.gooddate tmp.out || diff tmp.gooddate tmp.out
	sed 's/: /:/' tmp.in >tmp.822
	./canonhdr -m <tmp.822 >tmp.out
	cmp tmp.good tmp.out || diff tmp.good tmp.out
	NEWSTZ=3660 ; export NEWSTZ ; ./canonhdr -dm <ch.in >tmp.ch
	cmp ch.out tmp.ch || diff ch.out tmp.ch
	: namecheck.awk
	test " `echo | awk -f namecheck.awk`" = " empty name" ;
	test " `echo a b | awk -f namecheck.awk`" = " white space in name" ;
	test " `echo 'a	b' | awk -f namecheck.awk`" = " white space in name" ;
	test " `(echo a ; echo b) | awk -f namecheck.awk`" = " newline(s) in name" ;
	$(MX) namecheck.r
	sed -n '/^[^#]/p' namecheck.t >tmp.nc
	./namecheck.r <tmp.nc
	: a somewhat sketchy check of spacefor
	mkdir arts arts/in.coming
	$(HERE) ./spacefor 1024000 incoming >tmp.sf
	test `egrep '^[0-9]+$$' tmp.sf | wc -l` -eq 1 ;
	: the actual number should be about the free space in this filesystem,
	: in 1000KB units, less 5... unless you modify spacefor
	cat tmp.sf
	: report
	: this attempts to send test mail to you, or failing that, to
	: newsmaster...
	echo "nonurgentto=$${LOGNAME-newsmaster}" >report.ctl
	echo "urgentto=$${LOGNAME-newsmaster}" >>report.ctl
	echo "This is a test message verifying correctness of the" >tmp.m
	echo "configuration of C News.  Please disregard." >>tmp.m
	$(HERE) ./report -u "first test (urg)" <tmp.m
	$(HERE) ./report "second test (non-urg)" <tmp.m
	: success
	: but more needed... someday...

rclean:
	rm -f tmp.* report.ctl L*
	rm -rf arts

clean:	rclean
	rm -f *.o core $(BINS) spacefor queuelen sf.* dospacefor dbz.c
