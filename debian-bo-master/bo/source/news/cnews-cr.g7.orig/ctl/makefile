# =()<@<INCLUDE>@>()=
.include "../include/config.make"

BINS=
CTL=checkgroups newgroup rmgroup sendsys version
AUX=delsendsys delversion dodelayed newsreply
PROGS=$(BINS) $(CTL) $(AUX)
CDEST=$(NEWSBIN)/ctl
ADEST=$(NEWSBIN)/relay
ALL=$(PROGS) controlperm.eg

all:	$(ALL)
	$(MX) $(PROGS)

setup:
	$(TO) -n -e $(NEWSCTL) controlperm.eg

install:	$(PROGS)
	$(MKDIR) $(CDEST) $(ADEST)
	$(TO) $(CDEST) $(CTL)
	$(TO) $(ADEST) $(AUX)

cmp:	$(ALL)
	@$(IN) $(CDEST) $(CTL)
	@$(IN) $(ADEST) $(AUX)
	@$(IN) -i -e $(NEWSCTL) controlperm.eg

# stuff for regression test
UTIL=../util/gngp ../util/canonhdr ../util/newshostname ../util/now \
	../util/mkpdir
AWK=../util/namecheck.awk ../util/canonsys.awk

../util/gngp:
	cd ../util ; $(MAKE) gngp

../util/canonhdr:
	cd ../util ; $(MAKE) canonhdr

../util/newshostname:
	cd ../util ; $(MAKE) newshostname

../util/now:
	cd ../util ; $(MAKE) now

../util/mkpdir:
	cd ../util ; $(MAKE) mkpdir

r:	rclean $(PROGS) $(UTIL)
	touch controlperm active active.times newsgroups
	echo "alt any nr nv" >>controlperm
	echo "comp tale@uunet.uu.net nr yv" >>controlperm
	echo "comp.sys.ibm.pc.soundcard 000 001 y" >>active
	echo "comp.sys.ibm.pc.soundcard 0 nobody" >>active.times
	echo "comp.sys.ibm.pc.soundcard something or other" >>newsgroups
	echo "sci.space 000 001 y" >>active
	echo "sci.space 0 somebody" >>active.times
	echo "sci.space spacey stuff" >>newsgroups
	mkdir bin
	cp $(UTIL) bin
	echo '( echo $$* ; echo ::: ; cat ) >>'`pwd`/reports >bin/report
	$(MX) bin/*
	cp $(AWK) .
	echo "site" >whoami
	echo "ME:comp" >sys
	: newgroup
	cp active active.good
	echo "comp.lang.beta 0000000000 00001 y" >>active.good
	cp active.times at.good
	echo "comp.lang.beta 0 tale@uunet.uu.net (David C Lawrence)" >>at.good
	cp newsgroups newsgroups.ok
	egrep '^comp.lang.beta	' reg/n.1 >>newsgroups.ok
	$(HERE) ./newgroup comp.lang.beta <reg/n.1 >out.tmp 2>err.tmp
	test ! -s out.tmp ;
	test ! -s err.tmp ;
	cmp active.good active || diff active.good active
	sed 's/ [0-9][0-9]* / 0 /' active.times >at.tmp
	cmp at.good at.tmp || diff at.good at.tmp
	cmp newsgroups.ok newsgroups || diff newsgroups.ok newsgroups
	cmp reg/n.1r reports || diff reg/n.1r reports
	test -d arts/comp/lang/beta ;
	test -r arts/comp/lang/beta/.overview ;
	rm -f reports
	: newgroup for status change
	sed '/^comp.lang.beta/s/y$$/m/' active >active.good
	sed 's/ [0-9][0-9]* / 0 /' active.times >at.good
	cp newsgroups newsgroups.ok
	$(HERE) ./newgroup comp.lang.beta moderated <reg/n.2 >out.tmp 2>err.tmp
	test ! -s out.tmp ;
	test ! -s err.tmp ;
	cmp active.good active || diff active.good active
	sed 's/ [0-9][0-9]* / 0 /' active.times >at.tmp
	cmp at.good at.tmp || diff at.good at.tmp
	cmp newsgroups.ok newsgroups || diff newsgroups.ok newsgroups
	cmp reg/n.2r reports || diff reg/n.2r reports
	rm -f reports
	: rmgroup
	egrep -v soundcard active >active.good
	egrep -v soundcard active.times >at.good
	egrep -v soundcard newsgroups >newsgroups.ok
	$(HERE) ./rmgroup comp.sys.ibm.pc.soundcard <reg/r.1 >out.tmp 2>err.tmp
	test ! -s out.tmp ;
	test ! -s err.tmp ;
	cmp active.good active || diff active.good active
	cmp at.good active.times || diff at.good active.times
	cmp newsgroups.ok newsgroups || diff newsgroups.ok newsgroups
	cmp reg/r.1r reports || diff reg/r.1r reports
	test " `cat dirs.tbd`" = " comp/sys/ibm/pc/soundcard" ;
	: success

rclean:
	rm -f controlperm active active.times newsgroups dirs.tbd reports
	rm -f *.good *.tmp namecheck.awk canonsys.awk whoami sys newsgroups.ok
	rm -rf bin arts

clean:	rclean
	rm -f core a.out *.o $(BINS)
