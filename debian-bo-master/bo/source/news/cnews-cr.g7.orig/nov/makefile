# =()<@<INCLUDE>@>()=
.include "../include/config.make"

BINS=mkov expovguts ovsplit
PROGS=$(BINS) domkov expov initov
DEST=$(NEWSBIN)/nov
ALL=$(PROGS)

all:	$(ALL)
	$(MX) $(PROGS)

install:	$(PROGS)
	$(MKDIR) $(DEST)
	$(TO) $(DEST) $(PROGS)

setup:	$(ALL)

cmp:	$(ALL)
	@$(IN) $(DEST) $(PROGS)

$(BINS):	$(LIB)

mkov:	mkov.o
	$(CC) $(LDFLAGS) mkov.o $(LIBS) -o $@

expovguts:	expovguts.o
	$(CC) $(LDFLAGS) expovguts.o $(LIBS) -o $@

ovsplit:	ovsplit.o
	$(CC) $(LDFLAGS) ovsplit.o $(LIBS) -o $@

r:	$(ALL) rclean
	echo "foo 10 2 y" >active
	echo "bar 5 4 y" >>active
	echo "bletch 0 1 y" >>active
	mkdir bin arts arts/foo arts/bar
	echo "exit 0" >bin/lock
	echo "exit 0" >bin/unlock
	cp $(BINS) bin
	$(MX) bin/*
	cp art1 arts/foo/2
	cp art2 arts/foo/10
	cp art2 arts/bar/4
	cp art3 arts/bar/5
	: initov
	$(HERE) ./initov
	cmp arts.foo arts/foo/.overview || diff arts.foo arts/foo/.overview
	cmp arts.bar arts/bar/.overview || diff arts.bar arts/bar/.overview
	test ! -d bletch ;
	: domkov
	rm -f arts/*/.overview
	for f in art[1-3] ; do awk -f reghdr $$f ; done >headers
	$(HERE) ./domkov
	test ! -f headers.old ;
	test ! -s headers ;
	cmp arts.foo arts/foo/.overview || diff arts.foo arts/foo/.overview
	cmp arts.bar arts/bar/.overview || diff arts.bar arts/bar/.overview
	: expov
	rm -f arts/foo/2
	$(HERE) ./expov
	sed '/^2	/d' arts.foo >tmp.foo
	cmp tmp.foo arts/foo/.overview || diff tmp.foo arts/foo/.overview
	cmp arts.bar arts/bar/.overview || diff arts.bar arts/bar/.overview
	test ! -d bletch ;
	: success!

rclean:
	rm -f active headers headers.old L* tmp.* active.ovt
	rm -rf bin arts

clean:	rclean
	rm -f gmon.out a.out core *.o thread mkov expovguts ovsplit
