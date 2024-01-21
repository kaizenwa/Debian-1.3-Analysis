# =()<@<INCLUDE>@>()=
.include "../include/config.make"

DEST=$(NEWSBIN)/batch

BINS=batcher bencode c7encode
PROGS=$(BINS) batchih batchsm batchsplit compcun viainews viauux \
	sendbatches viamail \
	viarsh batchcheck batchra \
	batchsplitter bpfind nencode usenntpxmit
BP=batchparms.eg
ALL=$(PROGS) $(BP)

all:	$(ALL)
	$(MX) $(PROGS)

install:	$(ALL)
	$(MKDIR) $(DEST)
	$(TO) $(DEST) $(PROGS)

setup:	$(BP)
	$(MKDIR) $(NEWSARTS)/out.going $(NEWSARTS)/out.nntp
	$(TO) -n -e $(NEWSCTL) $(BP)

cmp:	$(ALL)
	@$(IN) $(DEST) $(PROGS)
	@$(IN) -i -e $(NEWSCTL) $(BP)
	: checking batchparms format
	@awk '$$0 !~ /^#/ && $$2 ~ /00/' $(NEWSCTL)/batchparms >bp.00 ; true
	@test ! -s bp.00 ;
	@rm -f bp.00
	: okay

$(BINS):	$(LIB)

batcher:	batcher.o
	$(CC) $(LDFLAGS) batcher.o $(LIBS) -o $@

c7encode:	c7encode.o
	$(CC) $(LDFLAGS) c7encode.o $(LIBS) -o $@

bencode:	bencode.o crctab.o
	$(CC) $(LDFLAGS) bencode.o crctab.o $(LIBS) -o $@

bencode.o:	coder.h

rsetup:	test.1 test.2 test.3 test.good bin arts bp.huey bp.nobody

test.1:
	echo 'a test' >$@

test.2:
	echo 'another test' >$@

test.3:
	cat batcher.c batcher.c batcher.c batcher.c batcher.c >$@

test.good:
	echo "#! rnews `wc -c test.1 | awk '{print $$1}'`" >test.good
	cat test.1 >>test.good
	echo "#! rnews `wc -c test.2 | awk '{print $$1}'`" >>test.good
	cat test.2 >>test.good
	( echo --- ; cat test.good ) >test.out1
	( echo +++ ; echo 2 ; cat test.good ) >test.out2
	echo "#! rnews `wc -c test.3 | awk '{print $$1}'`" >test.temp
	cat test.3 >>test.temp
	cat test.temp >>test.good
	( echo --- ; cat test.temp ) >>test.out1

bin:	$(PROGS)
	mkdir bin
	echo 'cat' >bin/munch
	echo "( echo --- ; cat ) >>`pwd`/output" >bin/xmit
	echo 'echo 2 ; cat' >bin/munch2
	echo "( echo +++ ; cat ) >>`pwd`/output2" >bin/xmit2
	echo 'echo 4' >bin/queuelen
	echo 'echo 99' >bin/spacefor
	echo '' >bin/lock
	echo '' >bin/unlock
	echo 'echo $$* ; echo === ; cat ; echo ===' >bin/report
	echo 'cat >>$$NEWSSITEDIR/togo.leftover' >bin/staller
	$(MX) bin/*

arts:
	mkdir arts
	mkdir arts/out.going arts/out.going/foo arts/out.going/bar

bp.huey:
	echo "bpokay=y" >$@
	echo "bpsite='huey'" >>$@
	echo "bpclass='u'" >>$@
	echo "bpsize='500000'" >>$@
	echo "bpmax='750000'" >>$@
	echo "bpqueue='20'" >>$@
	echo "bpcmd='batcher | gzip -9 | viauux -d gunzip'" >>$@

bp.nobody:
	echo "bpokay=y" >$@
	echo "bpsite='/default/'" >>$@
	echo "bpclass='u'" >>$@
	echo "bpsize='500000'" >>$@
	echo "bpmax='1500000'" >>$@
	echo "bpqueue='20'" >>$@
	echo "bpcmd='batcher | compcun | viauux'" >>$@

r:	all rclean rsetup r.bpf r.split r.batcher r.sendwho r.sendbatches
	: success!

r.bpf:	bpfind
	$(MX) bpfind
	$(HERE) ./bpfind . huey batchparms.eg >bp.tmp
	cmp bp.huey bp.tmp || diff bp.huey bp.tmp
	$(HERE) ./bpfind u huey batchparms.eg >bp.tmp
	cmp bp.huey bp.tmp || diff bp.huey bp.tmp
	$(HERE) ./bpfind u nobody batchparms.eg >bp.tmp
	cmp bp.nobody bp.tmp || diff bp.nobody bp.tmp
	$(HERE) ./bpfind u donald batchparms.eg >bp.tmp
	test " `cat bp.tmp`" = " bpokay=x" ;

r.split:
	echo x 1 >togo
	echo a 1 >>togo
	echo b 1 >>togo
	echo c 1 >>togo
	echo d 1 >>togo
	echo e 1 >>togo
	echo f 1 >>togo
	echo g 1 >>togo
	echo h 1 >>togo
	echo i 1 >>togo
	echo j 3 >>togo
	echo k 20 >>togo
	echo l 99999 >>togo
	cp togo togo.save
	( cd arts ; touch x a b c d e f g h i j k l )
	$(HERE) ./batchsplit -X 16 1000000
	test -r togo ;
	test ! -s togo ;
	test " `cat togo.1`" = " x 1" ;
	test " `cat togo.2`" = " a 1" ;
	test " `cat togo.3`" = " b 1" ;
	test " `cat togo.4`" = " c 1" ;
	test " `cat togo.5`" = " d 1" ;
	test " `cat togo.6`" = " e 1" ;
	test " `cat togo.7`" = " f 1" ;
	sed '1,7d' togo.save >togo.sp
	cat togo.next togo.more >togo.tmp
	cmp togo.tmp togo.sp
	test " `wc -l <togo.next`" -eq 5 ;
	mv togo.save togo
	rm togo.*
	cp togo togo.save
	rm arts/x
	$(HERE) ./batchsplit -X 16 1000000
	test " `cat togo.1`" = " a 1" ;
	test " `cat togo.2`" = " b 1" ;
	test " `cat togo.3`" = " c 1" ;
	test " `cat togo.4`" = " d 1" ;
	test " `cat togo.5`" = " e 1" ;
	test " `cat togo.6`" = " f 1" ;
	test " `cat togo.7`" = " g 1" ;
	sed '1,8d' togo.save >togo.sp
	cat togo.next togo.more >togo.tmp
	cmp togo.tmp togo.sp
	test " `wc -l <togo.next`" -eq 4 ;

r.batcher:
	echo `pwd`/test.1 >togo
	echo ./test.2 >>togo
	cp test.2 arts
	echo `pwd`/test.3 666 >>togo
	./batcher togo >test.out
	cmp test.out test.good
	./batcher <togo >test.out
	cmp test.out test.good

r.sendwho:
	echo 'foo u 500 10 batcher | munch | xmit' >batchparms
	$(HERE) ./sendbatches -d >test.out
	echo foo >test.who
	cmp test.out test.who
	echo '/default/ u 10000 5 batcher | munch2 | xmit2' >>batchparms
	$(HERE) ./sendbatches -d | sort >test.out
	( echo foo ; echo bar ) | sort >test.who
	cmp test.out test.who
	$(HERE) ./sendbatches -d bletch >test.out
	echo bletch >test.who
	cmp test.out test.who
	egrep -v '^foo' batchparms >bp.n
	echo "bar n 10000 - usesomething" >>bp.n
	$(HERE) ./sendbatches -f `pwd`/bp.n -d -c n >test.out
	test " `cat test.out`" = " bar" ;

r.sendbatches:
	$(HERE) ./sendbatches
	test ! -s output ;
	test ! -s output2 ;
	echo `pwd`/test.1 200 >arts/out.going/foo/togo
	echo `pwd`/test.2 200 >>arts/out.going/foo/togo
	echo `pwd`/test.3 200 >>arts/out.going/foo/togo
	echo `pwd`/test.1 4000 >arts/out.going/bar/togo
	echo `pwd`/test.2 4000 >>arts/out.going/bar/togo
	echo `pwd`/test.3 4000 >>arts/out.going/bar/togo
	echo `pwd`/test.3 4000 >test.left
	echo 'foo u! 500 10 batcher | munch | xmit' >test.bp
	$(HERE) ./sendbatches -f `pwd`/test.bp
	test ! -s output ;
	test ! -s output2 ;
	$(HERE) ./sendbatches
	cmp output test.out1
	cmp output2 test.out2
	test ! -s arts/out.going/foo/togo ;
	test ! -s arts/out.going/bar/togo ;
	cmp test.left arts/out.going/bar/togo.next
	sed '/^$$/q' batchlog | sed '1d;$$d' | sed '$$d' >test.log
	test ! -s test.log ;
	sed '1,/^$$/d' batchlog | sed '/^$$/q' | sed '1d;$$d' | sed '$$d' >test.log
	sed 's;/[^ ]*/;/;' test.log >test.log2
	echo 'foo	backlog ? (disabled in /test.bp)' >test.logg
	cmp test.log2 test.logg
	sed '1,/^$$/d' batchlog | sed '1,/^$$/d' | sed '1d;$$d' | sed '$$d' >test.log
	echo 'bar	backlog 1 (batches flowing)' >test.bar
	cmp test.bar test.log
	echo 'bar n 5000000 - staller' >batchparms
	rm batchlog
	$(HERE) ./sendbatches
	sed '/^$$/d' batchlog | sed '1d;$$d' >test.log
	echo 'bar	backlog 1 (batching slow or stalled)' >test.bar
	cmp test.bar test.log

rclean:
	rm -rf bin arts
	rm -f test.* togo togo.* batchparms batchlog output output2
	rm -f [a-l] bp.*

clean:	rclean
	rm -f *.o $(BINS) bp.00
