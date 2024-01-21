MORECFLAGS=-I. -DFLUSHEVERY=6
# =()<@<INCLUDE>@>()=
.include "../include/config.make"

BINS=relaynews
PROGS=$(BINS) relayrun
DEST=$(NEWSBIN)/relay
ALL=$(PROGS)

OBJ=relaynews.o active.o article.o control.o \
	fileart.o fsys.o hdrdefs.o hdrparse.o \
	hdrmunge.o history.o io.o procart.o sys.o transmit.o trbatch.o \
	ihave.o morefds.o

all:	$(ALL)
	$(MX) $(PROGS)

setup:

install:	$(PROGS)
	$(MKDIR) $(DEST)
	$(TO) $(DEST) $(PROGS)
	: any relay/staleness file is obsolete and will foul things up
	rm -f $(DEST)/staleness

cmp:	$(PROGS)
	@$(IN) $(DEST) $(PROGS)
	: check that relaynews is NOT set-uid
	test " `ls -l $(DEST)/relaynews | awk '$$1 ~ /s/`" = " " ;
	: check that obsolete staleness program is NOT present
	test ! -f $(DEST)/staleness ;

relaynews: $(OBJ) $(LIB)
	$(CC) $(LDFLAGS) $(OBJ) $(LIBS) -o $@

r:	$(PROGS)
	$(MX) regress/regress
	(cd regress; sh regress)

rclean:
	rm -rf regress/tmp

clean:	rclean
	rm -f core a.out relaynews *.o	
