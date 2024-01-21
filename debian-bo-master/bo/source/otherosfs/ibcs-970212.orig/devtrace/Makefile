CFLAGS = -D__KERNEL__=1 \
	-I../include -Wall -Wstrict-prototypes \
	-O2 -fomit-frame-pointer -m486


all:	devtrace

install:


devtrace: devtrace.c
	$(CC) $(CFLAGS) -c -o devtrace devtrace.c

clean:
	rm -f devtrace core *.o *.a *.s .depend

dep:
	$(CPP) -M -I../include *.c > .depend

dummy:

#
# include a dependency file if one exists
#
ifeq (.depend,$(wildcard .depend))
include .depend
endif
