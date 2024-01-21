# =()<@<INCLUDE>@>()=
.include "../include/config.make"

OBJS=ascingmtime.o awksplit.o cfgetln.o closeall.o dateabstoks.o dateconv.o \
	datereltoks.o dogets.o efopen.o emalloc.o error.o \
	fgetfln.o fgetmfs.o getabsdate.o getindate.o \
	getreldate.o hash.o hdbm.o mkdirs.o nfclose.o qmktime.o split.o \
	stdfdopen.o warning.o
ALL=$(OBJS)

u:	$(ALL)
	$(UPDATE) $(OBJS)

o:	$(OBJS)

cmp:	$(ALL)
	$(LIBCMP) $(OBJS)

clean:
	rm -f *.o *.a y.*.h y.*.c
