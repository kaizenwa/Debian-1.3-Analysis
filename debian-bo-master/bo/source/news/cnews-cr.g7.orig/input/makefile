# =()<@<INCLUDE>@>()=
.include "../include/config.make"

BINS=c7decode bdecode newsspool
RCVRS=rnews cunbatch
PROGS=$(BINS) newsrun newsrunning recenews recpnews $(RCVRS)
ALL=$(PROGS)
DEST=$(NEWSBIN)/input
DBIN=$(NEWSBIN)/decompressors
DECS=c7decode bdecode gunzip
PGMS=$(PROGS) $(DECS)

BATCH=../batch

all:	$(ALL)
	$(MX) $(PGMS)

install:	$(PROGS)
	$(MKDIR) $(DEST) $(DBIN)
	rm -f $(DEST)/newsspool
	$(TO) $(DEST) $(PROGS)
	$(TO) $(RBIN) $(RCVRS)
	$(TO) $(DBIN) $(DECS)
	: "and newsspool needs to be made setuidgid-news"

setup:
	$(MKDIR) $(NEWSARTS)/in.coming $(NEWSARTS)/in.coming/bad

cmp:	$(ALL)
	@$(IN) $(DEST) $(PROGS)
	@$(IN) $(RBIN) $(RCVRS)
	@$(IN) $(DBIN) $(DECS)
	@$(CF) $(UID) $(GID) '^-r.s..s..x' $(DEST)/newsspool

$(BINS):	$(LIB)

newsspool:	newsspool.o
	$(CC) $(LDFLAGS) newsspool.o $(LIBS) -o $@

c7decode:	c7decode.o
	$(CC) $(LDFLAGS) c7decode.o $(LIBS) -o $@

bdecode:	bdecode.o crctab.o
	$(CC) $(LDFLAGS) bdecode.o crctab.o $(LIBS) -o $@

crctab.c:	$(BATCH)/crctab.c
	cp $(BATCH)/crctab.c $@

bdecode.o:	bdecode.c $(BATCH)/coder.h
	$(CC) -c $(CFLAGS) -I$(BATCH) bdecode.c

cunbatch:	rnews
	cp rnews $@

RFILES=test.1 test.2 test.2p test.3 test.3c test.3j test.3p test.n1\
	test.n2 test.out test.out2

test.1:
	echo '#! rnews' >$@
	echo 'here is a phony first batch' >>$@

test.2:	test.2p
	: compress tends to return silly exit status for tiny inputs
	-compress -b12 <test.2p >$@

test.2p:
	echo '#! rnews' >$@
	echo 'here is a phony second batch' >>$@

test.3:	test.3c
	( echo '#! cunbatch' ; cat test.3c ) >$@

test.3c:	test.3p
	: compress tends to return silly exit status for tiny inputs
	-compress -b12 <test.3p >$@

test.3p:
	echo '#! rnews' >$@
	echo 'here is a phony third batch' >>$@

test.3j:	test.3p
	( echo junk ; cat test.3p ) >$@

test.n1:
	: compress tends to return silly exit status for tiny inputs
	-true | compress -b12 >$@

test.n2:	test.n1
	( echo '#! cunbatch' ; cat test.n1 ) >$@

test.out:
	echo '#! rnews' >$@
	echo 'here is a phony first batch' >>$@
	echo '#! rnews' >>$@
	echo 'here is a phony second batch' >>$@
	echo '#! rnews' >>$@
	echo 'here is a phony third batch' >>$@

test.out2:
	echo '#! rnews' >$@
	echo 'here is a phony second batch' >>$@
	echo '#! rnews' >>$@
	echo 'here is a phony first batch' >>$@
	echo '#! rnews' >>$@
	echo 'here is a phony third batch' >>$@

rsetup:	$(ALL) rclean
	$(MX) $(PGMS)
	mkdir bin
	echo "shift;shift;shift;cat \$$* >>`pwd`/tmp.1" >bin/relaynews
	echo "echo 10" >bin/spacefor
	echo ':' >bin/lock
	echo ':' >bin/unlock
	echo ':' >bin/touchlock
	echo 'echo 10' >bin/sizeof
	echo ':' >bin/staleness
	echo ':' >bin/doexplode
	echo 'echo "$$*" ; cat >&2' >bin/report
	echo ':' >bin/domkov
	mkdir decompressors
	echo 'sed 1d' >decompressors/dejunk
	$(MX) bin/* decompressors/*
	mkdir arts arts/in.coming ;		# but not bad, yet

AIN=arts/in.coming

r:	rsetup $(RFILES)
	$(HERE) ./rnews <test.1
	cmp $(AIN)/* test.1
	expr $(AIN)/* : '.*\.t' >junk
	sleep 2 ;		# must delay for sake of "ls -t"!
	$(HERE) ./rnews <test.2
	$(HERE) ./rnews <test.n1
	$(HERE) ./rnews <test.n2
	test `ls $(AIN) | wc -l` -eq 2 ;
	cmp `ls -t $(AIN) | sed -n '1s;^;$(AIN)/;p'` test.2
	expr `ls -t $(AIN) | sed -n '1s;^;$(AIN)/;p'` : '.*\.Z' >junk
	sleep 2
	$(HERE) ./rnews <test.3
	cmp `ls -t $(AIN) | sed -n '1s;^;$(AIN)/;p'` test.3c
	expr `ls -t $(AIN) | sed -n '1s;^;$(AIN)/;p'` : '.*\.Z' >junk
	$(HERE) ./newsrunning off
	test -r $(AIN)/stop ;
	$(HERE) ./newsrunning on
	test ! -r $(AIN)/stop ;
	mkdir $(AIN)/bad
	$(HERE) ./newsrun '-#'
	cmp tmp.1 test.out
	test " `echo $(AIN)/*`" = ' $(AIN)/bad' ;
	rm tmp.1
	$(HERE) ./rnews <test.1
	$(HERE) ./newsspool -g 4 <test.2 2>junk
	$(HERE) ./rnews -d dejunk <test.3j
	$(HERE) ./newsrun '-#'
	cmp tmp.1 test.out2
	test " `echo $(AIN)/*`" = ' $(AIN)/bad' ;
	: 'success!'

rclean:
	rm -f junk tmp.? dejunk $(RFILES) log errlog
	rm -rf arts bin decompressors

clean:	rclean
	rm -f *.o $(BINS) lint cunbatch crctab.c
