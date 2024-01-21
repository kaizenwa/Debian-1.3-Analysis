# =()<@<INCLUDE>@>()=
.include "../include/config.make"

BINS=explode
PROGS=$(BINS) doexplode
DEST=$(NEWSBIN)/relay
ALL=$(PROGS)

all:	$(ALL)
	$(MX) $(PROGS)

install:	$(ALL)
	$(MKDIR) $(DEST)
	$(TO) $(DEST) $(PROGS)

setup:
	$(MKDIR) $(NEWSARTS)/out.master

cmp:	$(ALL)
	@$(IN) $(DEST) $(PROGS)

$(BINS):	$(LIB)

trbatch.o:	../include/trbatch.h

explode:	explode.o trbatch.o morefds.o
	$(CC) $(LDFLAGS) -o $@ explode.o trbatch.o morefds.o $(LIBS)

lint:	explode.c trbatch.c
	$(LINT) $(LINTFLAGS) explode.c trbatch.c

r:	rclean $(PROGS)
	mkdir arts bin
	echo "exit 0" >bin/lock
	echo "exit 0" >bin/unlock
	echo "echo 5" >bin/sizeof
	echo "echo 1000" >bin/spacefor
	echo "( echo \"\$$*\" ; echo ::: ; cat ) >&2" >bin/report
	echo "echo 12345" >bin/now
	cp $(BINS) bin
	$(MX) bin/*
	echo hostb >whoami
	mkdir arts/out.master
	cp om.0 arts/out.master/0
	mkdir arts/out.going
	mkdir arts/out.going/foo arts/out.going/bar arts/out.going/baz
	$(HERE) ./doexplode >stdout 2>stderr
	test ! -s stdout ;
	cmp om.err stderr || diff om.err stderr
	test " `ls arts/out.master`" = " 0.12345" ;
	cmp om.0 arts/out.master/0.12345 || diff om.0 arts/out.master/0.12345
	cmp og.foo arts/out.going/foo/togo || diff og.foo arts/out.going/foo/togo
	cmp og.bar arts/out.going/bar/togo || diff og.bar arts/out.going/bar/togo
	cmp og.baz arts/out.going/baz/togo || diff og.baz arts/out.going/baz/togo
	rm -f arts/out.master/0.12345 arts/out.going/*/togo stdout stderr
	egrep -v nope om.0 >arts/out.master/0
	$(HERE) ./doexplode >stdout 2>stderr
	test ! -s stdout ;
	test ! -s stderr ;
	test " `ls arts/out.master`" = " " ;
	cmp og.foo arts/out.going/foo/togo || diff og.foo arts/out.going/foo/togo
	cmp og.bar arts/out.going/bar/togo || diff og.bar arts/out.going/bar/togo
	cmp og.baz arts/out.going/baz/togo || diff og.baz arts/out.going/baz/togo
	: success

clean:	rclean
	rm -f *.o *.out core explode

rclean:
	rm -f whoami stdout stderr
	rm -rf arts bin
