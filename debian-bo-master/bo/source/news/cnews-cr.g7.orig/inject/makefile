MORECFLAGS=-I../relay -I.

# =()<@<INCLUDE>@>()=
.include "../include/config.make"

BINS=actflag defaults
PROGS=$(BINS) defhdrs.awk inews lines modroute injnews pnews tear
ALL=$(PROGS)
UI=inews injnews
DEST=$(NEWSBIN)/inject

RELAYBITS=../relay/active.o
RBITNAMES=active.o

all:	$(ALL)
	$(MX) $(PROGS)

setup:	$(DEST)/inews inews
	$(TO) $(NEWSCTL) inews

install:	$(PROGS)
	$(MKDIR) $(DEST)
	$(TO) $(DEST) $(PROGS)

ui:	$(UI)
	$(MKDIR) $(UIBIN)
	$(TO) $(UIBIN) $(UI)

cmp:
	@$(IN) $(DEST) $(PROGS)
	@$(IN) $(UIBIN) $(UI)
	@$(IN) $(NEWSCTL) inews

$(BINS):	$(LIB)

$(RELAYBITS):
	cd ../relay ; $(MAKE) $(RBITNAMES)

actflag:	actflag.o $(RELAYBITS)
	$(CC) $(LDFLAGS) actflag.o $(RELAYBITS) $(LIBS) -o $@

DEFO=defaults.o fullname.o getmypwent.o intcode.o
defaults:	$(DEFO)
	$(CC) $(LDFLAGS) $(DEFO) $(LIBS) -o $@

clean:	rclean
	rm -f core a.out *.o actflag defaults

rclean:
	rm -rf regress/tmp

../util/canonhdr:
	cd ../util ; $(MAKE) canonhdr

r:	rclean $(PROGS) ../util/canonhdr
	mkdir regress/tmp regress/tmp/inject
	cp $(PROGS) ../util/canonhdr regress/tmp/inject
	$(MX) regress/regress regress/master/run
	$(HERE) cd regress ; NEWSTZ=3660 ; export NEWSTZ ; ./regress
	
# header dependencies follow
