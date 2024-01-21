DEFINES=-I../include
# -g
CFLAGS=$(DEFINES) -O
LINTFLAGS=-ha $(DEFINES)

all: libnov.a

libnov.a: lib.c
	$(CC) $(CFLAGS) -c lib.c && mv lib.o $@

lint: lib.c
	lint $(LINTFLAGS) -u lib.c

clean:
	rm -f gmon.out a.out core *.[oa]
