# =()<@<INCLUDE>@>()=
.include "../include/config.make"

BINS=readnews
PROGS=$(BINS) checknews postnews
DEST=$(NEWSBIN)/inject
CTL=readnews.help readnews.ctl postdefltgroup
ALL=$(PROGS) $(CTL)

all:	$(ALL)
	$(MX) $(PROGS)

setup:	$(CTL)
	$(TO) -n -e $(NEWSCTL) $(CTL)

install:	$(PROGS)
	$(MKDIR) $(DEST)
	$(TO) $(DEST) postnews

readpostcheck:	$(PROGS)
	$(MKDIR) $(UIBIN)
	$(TO) $(UIBIN) $(PROGS)

cmp:	$(ALL)
	@$(IN) $(DEST) postnews
	@$(IN) -i -e $(NEWSCTL) $(CTL)
	@$(IN) -x $(UIBIN) $(PROGS)

PARTS=header.o readnews.o funcs.o active.o newsrc.o
$(PARTS):	defs.h
$(BINS):	$(LIB)

readnews:	$(PARTS)
	$(CC) $(LDFLAGS) $(PARTS) $(LIBS) -o $@

r rclean:	$(ALL)

clean:
	rm -f *.o core $(BINS) .newsrc
