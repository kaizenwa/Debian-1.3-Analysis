# =()<@<INCLUDE>@>()=
.include "../include/config.make"



BINS=expire
PROGS=$(BINS) upact doexpire expireiflow
DEST=$(NEWSBIN)/expire
ALL=$(PROGS) explist.eg
UPACTOPT=
UPACTBARF=

all:	$(ALL)
	$(MX) $(PROGS)

install:	$(PROGS)
	$(MKDIR) $(DEST)
	$(TO) $(DEST) $(PROGS)

setup:	explist.eg
	$(TO) -n -e $(NEWSCTL) explist.eg

cmp:	$(ALL)
	@$(IN) $(DEST) $(PROGS)
	@$(IN) -i -e $(NEWSCTL) explist.eg

$(BINS):	$(LIB)

expire: expire.o
	$(CC) $(LDFLAGS) expire.o $(LIBS) -o $@

histinfo: histinfo.o
	$(CC) $(LDFLAGS) histinfo.o $(LIBS) -o $@

histslash:	histslash.o
	$(CC) $(LDFLAGS) histslash.o $(LIBS) -o $@

lint:	expire.c
	lint $(LINTFLAGS) expire.c 2>&1

explist.regw:
	rm -f $@
	echo "/expired/	x	0.08	-" >>$@
	echo '/bounds/	x	0.01-0.1-0.11	- ' >>$@
	echo '#comment' >>$@
	echo >>$@
	echo ' ' >>$@
	echo "bar,!bar.ugh	x	0.05	`pwd`/arch2" >>$@
	echo 'mod	m	0.1	@' >>$@
	echo 'mod	u	0.05	-' >>$@
	echo 'urp	x	0.01-0.05-0.1	-' >>$@
	echo "bletch	x	0.1	=`pwd`/arch3/bletch" >>$@
	echo 'unused	x	1	-' >>$@
	echo 'all	x	0.1	@' >>$@

explist.reg:	explist.regw
	rm -f $@
	egrep -v '^unused' explist.regw >$@

UTILS=../util/dbz ../util/mkpdir ../maint/checkactive ../util/gngp
AUTILS=../util/canonsys.awk ../util/namecheck.awk

../util/dbz:
	cd ../util ; $(MAKE) dbz

../util/mkpdir:
	cd ../util ; $(MAKE) mkpdir

../util/gngp:
	cd ../util ; $(MAKE) gngp

../maint/checkactive:
	cd ../maint ; $(MAKE) checkactive

BARFS=active.barf active.nobarf
AB=active.$(UPACTBARF)barf

active.barf:
	echo 'barf.puke 01111 01112 y' >$@

active.nobarf:
	touch $@

# setup for regression test
rsetup:	$(PROGS) $(UTILS) $(AUTILS) explist.reg explist.regw $(BARFS)
	$(MX) $(PROGS) dircheck
	>history.pag
	>history.dir
	mkdir bin
	cp $(UTILS) bin
	echo 'exit 0' >bin/lock
	echo 'exit 0' >bin/unlock
	echo 'echo 10' >bin/spacefor
	echo 'echo fred' >bin/newshostname
	$(MX) bin/*
	cp $(AUTILS) .
	echo "ME:all" >sys
	mkdir arts arts/foo arts/bar arts/bar/ugh arts/urp arch arch2
	mkdir arts/mod arts/mod/mod arts/mod/unmod arch3 arch3/bletch
	mkdir arts/bletch arts/lost+found arts/barf arts/barf/puke
	mkdir arts/control arts/junk
	echo 'foo 00103 00001 y ' >>active
	echo 'foo 00103 00098 y' >>active.after
	echo 'bar 00099 00001 m' >>active
	echo 'bar 00099 00100 m' >>active.after
	echo 'bar.ugh 00099 00001 m' >>active
	echo 'bar.ugh 00099 00099 m' >>active.after
	echo 'urp 00099 00001 n' >>active
	echo 'urp 00099 00007 n' >>active.after
	echo 'mod.mod 00013 00001 m' >>active
	echo 'mod.mod 00013 00013 m' >>active.after
	echo 'mod.unmod 00016 00001 y' >>active
	echo 'mod.unmod 00016 00016 y' >>active.after
	echo 'bletch 00099 00001 y' >>active
	echo 'bletch 00100 00001 y' >>active.after
	echo 'control 00000 00001 y' >>active
	echo 'control 00000 00001 y' >>active.after
	echo 'junk 00000 00001 y' >>active
	echo 'junk 00000 00001 y' >>active.after
	cat $(AB) >>active
	cat $(AB) >>active.after
	echo nevermore >arts/lost+found/1
	echo '<wont1>	9999~-	foo/98' >>history.proto
	echo :foo/98: >arts/foo/98
	echo 'MeSsAge-ID: <wont1>' >>arts/foo/98
	echo '<will2>	1000~-	foo/99' >>history.proto
	echo foo/99 >arts/foo/99
	echo '<will3>	1000~100	foo/100' >>history.proto
	echo foo/100 >arts/foo/100
	echo '<two4>	100	bar/4' >>history.proto
	echo :bar/4: >arts/bar/4
	echo 'SuBjeCt: yes' >>arts/bar/4
	echo '<will5>	100~	bar.ugh/5' >>history.proto
	echo :bar/ugh/5: >arts/bar/ugh/5
	echo >>arts/bar/ugh/5
	echo 'Subject: no' >>arts/bar/ugh/5
	echo '<gone6>	100~-	urp/6' >>history.proto
	echo urp/6 >arts/urp/6
	echo '<wont7>	9999~9999~xx~a	urp/7' >>history.proto
	echo :urp/7: >arts/urp/7
	echo 'mEssaGe-id: <wont7>' >>arts/urp/7
	echo 'exPiRes: 9999' >>arts/urp/7
	echo '<gone8>	100~20000	urp/8' >>history.proto
	echo urp/8 >arts/urp/8
	echo '<gone9>	100~-	urp/9' >>history.proto
	echo '<wont10>	1000~20000	foo/101' >>history.proto
	echo :foo/101: >arts/foo/101
	echo 'Message-ID: <wont10>' >>arts/foo/101
	echo 'Expires:  20000' >>arts/foo/101
	echo 'Supersedes: <wont1>' >>arts/foo/101
	echo >>arts/foo/101
	echo '<will11>	100~20000	foo/102' >>history.proto
	echo foo/102 >arts/foo/102
	echo '<will12>	100~-	mod.mod/12' >>history.proto
	echo mod/mod/12 >arts/mod/mod/12
	echo '<wont13>	2000	mod.mod/13' >>history.proto
	echo :mod/mod/13: >arts/mod/mod/13
	echo 'Message-ID: <wont13>' >>arts/mod/mod/13
	echo '<gone14>	100~-	mod.unmod/14' >>history.proto
	echo mod/unmod/14 >arts/mod/unmod/14
	echo '<gone15>	2000	mod.unmod/15' >>history.proto
	echo mod/unmod/15 >arts/mod/unmod/15
	echo '<wont16>	9999~-	mod.unmod/16' >>history.proto
	echo :mod/unmod/16: >arts/mod/unmod/16
	echo 'Message-ID:	<wont16>' >>arts/mod/unmod/16
	echo '<three17>	100~-	bletch/17' >>history.proto
	echo bletch/17 >arts/bletch/17
	egrep wont history.proto >history.after
	echo '<gone96>	5000~-' >>history.proto
	echo '<gone96>	5000~-' >>history.after
	echo '<gone98>	5000~-	urp/98' >>history.proto
	echo '<gone98>	5000~-' >>history.after
	echo urp/98 >arts/urp/98
	echo '<multi99>	5000~-	bar/99, bar.ugh/99  foo/103 urp/99' >>history.proto
	echo '<multi99>	5000~-	bar.ugh/99 foo/103' >>history.after
	echo :foo/103:bar/99:bar/ugh/99:urp/99: >arts/foo/103
	echo 'Message-ID:  <multi99>' >>arts/foo/103
	ln arts/foo/103 arts/bar/99
	ln arts/foo/103 arts/bar/ugh/99
	ln arts/foo/103 arts/urp/99
	cp history.proto history

# regression test machinery
RUN=$(HERE) ./expire -H `pwd` -a `pwd`/arch -n 10000 -B 256
FIXMIDFIELD='s/	[0-9]*~*/	~/;s/~	/~-	/;s/~xx~a//'
FIXPATHS="s;`pwd`;P;g"

# good-output files for regression check
goodt:	expire rsetup	# do NOT run this unless you know what you're doing
	$(RUN) -t explist.reg 2>&1 | sed $(FIXPATHS) >tgood
	chmod -w tgood
goodp:	expire setup	# do NOT run this unless you know what you're doing
	$(RUN) -p explist.reg >pgood 2>&1
	chmod -w pgood
doit:	expire		# for debugging
	echo "$(RUN) -t explist.reg" >$@
	$(MX) $@

# the regression test proper
r:	$(PROGS) rclean rsetup dircheck tgood pgood
	$(RUN) -c explist.regw >test.out 2>test.stderr
	test ! -s test.out ;
	cmp wgood test.stderr || diff wgood test.stderr
	: "okay, it's sane"
	$(RUN) -t explist.reg >test.out 2>&1
	sed $(FIXPATHS) test.out | cmp - tgood
	: "okay, it seems to know what it's doing -- now, can it do it?"
	$(RUN) -p explist.reg >test.out 2>test.stderr
	test ! -s test.stderr ;
	cmp test.out pgood
	test ! -f history.n ;
	cmp history.proto history.o
	egrep wont history.proto | ./dircheck arts
	egrep 'will|two|gone|three' history.proto | ./dircheck -n arts
	egrep will history.proto | ./dircheck arch
	egrep 'wont|two|gone|three' history.proto | ./dircheck -n arch
	egrep two history.proto | ./dircheck arch2
	egrep 'will|wont|gone|three' history.proto | ./dircheck -n arch2
	egrep three history.proto | ./dircheck arch3
	egrep 'will|wont|gone|two' history.proto | ./dircheck -n arch3
	test -f arts/foo/103 ;
	test -f arts/bar/ugh/99 ;
	test -f arch2/bar/99 ;
	test ! -f arts/urp/99 ;
	cmp history history.after
	test -s history.dir ;
	test -s history.pag ;
	: "that's it for expire, on to upact"
	echo "sed '/bletch/s/099/100/' active >active.up" >junk
	echo "mv active.up active" >>junk
	$(MX) junk
	$(HERE) ./upact $(UPACTOPT) -I `pwd`/junk '-#'
	test ! -s active.errs ;
	cmp active.after active.tmp || diff active.after active.tmp
	mv active.tmp active
	sed '/bletch/s/001 /101 /' active.after >active.after2
	$(HERE) ./upact $(UPACTOPT)
	cmp active.after2 active || diff active.after2 active
	sed '/^foo /s/103/009/' active.after2 >active
	$(HERE) ./upact -b $(UPACTOPT)
	cmp active.after2 active || diff active.after2 active
	: "success!"

rclean:
	rm -f junk history history.pag history.dir history.o active active.tmp
	rm -f history.n* *mon.out history.proto history.after test.out doit
	rm -f active.old active.new explist.reg lint active.after* test.stderr
	rm -f active.errs explist.regw $(BARFS)
	rm -f canonsys.awk namecheck.awk sys L*
	rm -rf arts arch arch2 arch3 bin

clean:	rclean
	rm -f *.o expire
