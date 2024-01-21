# =()<@<INCLUDE>@>()=
.include "../include/config.make"

OBJS=complain.o config.o fopenclex.o gethdr.o hostname.o lock.o ltoza.o \
	nemalloc.o ngmatch.o str3save.o string.o strlower.o \
	strsave.o time.o case.o mkinperm.o unprivileged.o \
	trbatcomm.o fnlockfile.o msgs.o fopenexcl.o rerror.o fakeftime.o
ALL=$(OBJS)

u:	$(ALL)
	$(UPDATE) $(OBJS)

o:	$(OBJS)

cmp:	$(ALL)
	$(LIBCMP) $(OBJS)

rerror.o:	rerror.c
	$(CC) -c $(CFLAGS) -I../relay rerror.c

clean:
	rm -f *.o
