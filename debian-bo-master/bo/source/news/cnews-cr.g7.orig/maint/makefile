# =()<@<INCLUDE>@>()=
.include "../include/config.make"

BINS=histinfo
NORMAL=$(BINS) newsdaily newswatch newsboot addgroup delgroup adddirs \
	act.to.times histfrom newsfrom addmissing newshist badexpiry \
	newsflag mkhistory locknews histdups logroll mergeactive \
	checkactive
CUSTOM=spaceshort
UI=cnewsdo
DPROGS=$(NORMAL) $(CUSTOM)
PROGS=$(DPROGS) $(UI)
DEST=$(NEWSBIN)/maint
ALL=$(PROGS)

all:	$(ALL)
	$(MX) $(PROGS)

install:	$(PROGS)
	$(MKDIR) $(DEST)
	$(TO) $(DEST) $(NORMAL)
	$(TO) -n $(DEST) $(CUSTOM)

ui:	$(UI)
	$(MKDIR) $(UIBIN)
	$(TO) $(UIBIN) $(UI)

setup:

cmp:	$(ALL)
	@$(IN) $(DEST) $(NORMAL)
	@$(IN) -i $(DEST) $(CUSTOM)
	@$(IN) $(UIBIN) $(UI)
	$(DEST)/checkactive -q

$(BINS):	$(LIB)

histinfo:	histinfo.o
	$(CC) $(LDFLAGS) histinfo.o $(LIBS) -o $@

# regression test machinery
FIXMIDFIELD='s/	[1-9][0-9]*~/	1~/'

../util/dbz:
	cd ../util ; $(MAKE) dbz

../util/gngp:
	cd ../util ; $(MAKE) gngp

rsetup:	$(PROGS) ../util/dbz ../util/gngp ../util/namecheck.awk ../util/canonsys.awk
	$(MX) $(PROGS)
	mkdir bin maint
	cp histinfo histdups maint
	cp ../util/dbz ../util/gngp bin
	echo 'exit 0' >bin/lock
	echo 'exit 0' >bin/unlock
	echo 'echo here' >bin/newshostname
	echo 'echo 0' >bin/now
	$(MX) maint/* bin/*
	cp ../util/namecheck.awk ../util/canonsys.awk .
	mkdir arts arts/foo arts/bar arts/bar/ugh arts/urp
	mkdir arts/bletch arts/lost+found
	mkdir arts/junk arts/control
	echo nevermore >arts/lost+found/1
	echo '<okay1>	9999~-	foo/1' >history.gbase
	echo 'MeSsAge-ID: <okay1>' >arts/foo/1
	echo '<okay2>	9999~-	bar.ugh/7' >>history.gbase
	echo 'Message-ID: <okay2>' >arts/bar/ugh/7
	echo '<cross2>	9999~-	bar.ugh/8 foo/2' >>history.gbase
	echo 'Message-ID: <cross2>' >arts/foo/2
	ln arts/foo/2 arts/bar/ugh/8
	echo '<cross4>	9999~-	bar.ugh/9 bar/13 foo/3 urp/2' >>history.gbase
	echo 'Message-ID: <cross4>' >arts/foo/3
	ln arts/foo/3 arts/bar/ugh/9
	ln arts/foo/3 arts/bar/13
	ln arts/foo/3 arts/urp/2
	echo '<link2>	9999~tomorrow	bar.ugh/10 foo/4' >>history.gbase
	echo 'Message-ID: <link2>' >arts/foo/4
	echo 'Expires: tomorrow' >>arts/foo/4
	ln arts/foo/4 arts/bar/ugh/10
	echo '<bar.ugh/11@trash>	0~0	bar.ugh/11' >>history.gbase
	echo >arts/bar/ugh/11
	echo '<bar.ugh/12@trash>	0~0	bar.ugh/12' >>history.gbase
	echo 'Expires: not~today' >arts/bar/ugh/12
	echo 'Message-ID: <legal>' >>arts/bar/ugh/12
	echo '<bar.ugh/13@trash>	0~0	bar.ugh/13' >>history.gbase
	echo 'Message-ID: illegal' >arts/bar/ugh/13
	sed $(FIXMIDFIELD) history.gbase | sort >history.good
	echo 'here/there:foo,bar,bletch/nonsense' >sys

r:	$(PROGS) rclean rsetup
	: "mkhistory"
	$(HERE) ./mkhistory >mkh.out 2>mkh.diag
	echo './mkhistory: (warning) empty/trash articles found, will expire at once' >mkh.good
	echo "./mkhistory:	(grep history file for '@trash' to see them)" >>mkh.good
	cmp mkh.good mkh.diag || diff mkh.good mkh.diag
	test ! -s mkh.out ;
	sed $(FIXMIDFIELD) history | sort >history.fixed
	cmp history.good history.fixed || diff history.good history.fixed
	test -r history.pag ;
	test -r history.dir ;
	: "addmissing"
	echo '<missing>	9999~-	bar.ugh/14' >>history.gbase
	echo 'Message-ID: <missing>' >arts/bar/ugh/14
	sed $(FIXMIDFIELD) history.gbase | sort >history.good
	sleep 2 ;	# ensure article looks older than addmissing startup
	$(HERE) ./addmissing -v >addm.out 2>addm.diag
	sed $(FIXMIDFIELD) history | sort >history.fixed
	cmp history.good history.fixed || diff history.good history.fixed
	sed $(FIXMIDFIELD) addm.out | sort >addm.fixed
	echo "./addmissing: found 1 articles missing from history:" >addm.good
	echo "<missing>	1~-	bar.ugh/14" >>addm.good
	cmp addm.good addm.fixed || diff addm.good addm.fixed
	test ! -s addm.diag ;
	: "cnewsdo, using addmissing as test command"
	echo '<missing2>	9999~-	bar.ugh/15' >>history.gbase
	echo 'Message-ID: <missing2>' >arts/bar/ugh/15
	sed $(FIXMIDFIELD) history.gbase | sort >history.good
	sleep 2 ;	# ensure article looks older than addmissing startup
	touch active
	$(HERE) ./cnewsdo addmissing >addm.out 2>addm.diag
	sed $(FIXMIDFIELD) history | sort >history.fixed
	cmp history.good history.fixed || diff history.good history.fixed
	test ! -s addm.out ;
	test ! -s addm.diag ;
	: quick verification that checkactive at least runs, not a good test...
	echo "foo 77 01 m" >active
	echo "bar 99 77 y" >>active
	echo "control 00 01 y" >>active
	echo "junk 00 01 y" >>active
	echo "bletch 0000000000 00001 =foo" >>active
	$(HERE) ./checkactive >ca.out 2>ca.diag
	test ! -s ca.out ;
	test ! -s ca.diag ;
	: give mergeactive a try
	echo "foo 0 nobody" >active.times
	echo "bar 0 nobody" >>active.times
	echo "control 0 nobody" >>active.times
	echo "junk 0 nobody" >>active.times
	echo "bletch 0 regression@here" >>active.times
	echo "foo 22 21 m" >active.new
	echo "bletch 777 002 =foo" >>active.new
	mv active active.good
	egrep -v bletch active.good >active
	mv active.times actimes.good
	egrep -v bletch actimes.good >active.times
	$(HERE) ./mergeactive active.new >ma.out 2>ma.diag
	test ! -s ma.out ;
	test ! -s ma.diag ;
	cmp active.good active || diff active.good active
	cmp actimes.good active.times || diff actimes.good active.times
	: "success!"

rclean:
	rm -f history history.pag history.dir active active.* active.times*
	rm -f history.n* history.* *.out *.diag *.good *.fixed actimes.*
	rm -f namecheck.awk canonsys.awk sys
	rm -rf arts maint bin

clean:	rclean
	rm -f *.o core $(BINS)
