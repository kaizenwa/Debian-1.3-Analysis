
PROGS	= posix gentest tpwdb radtest grouptest

SRCS = $(addsuffix .c,$(PROGS))
OBJS = $(addsuffix .o,$(PROGS))

# rules
junk:
	@echo "this is not a top-level Makefile"
	exit 1

%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@

all:	${PROGS}

posix: posix.o ../libpwdb/libpwdb.a
	$(CC) -o $@ $< ../libpwdb/libpwdb.a

tpwdb: tpwdb.o ../libpwdb/libpwdb.a
	${CC} -g -o $@ $< ../libpwdb/libpwdb.a # $(LOADLIBES)

radtest: radtest.o ../libpwdb/libpwdb.a
	${CC} -g -o $@ $< ../libpwdb/libpwdb.a # $(LOADLIBES)

gentest: gentest.o ../libpwdb/libpwdb.a
	${CC} -g -o $@ $< ../libpwdb/libpwdb.a # $(LOADLIBES)

grouptest: grouptest.o ../libpwdb/libpwdb.a
	${CC} -g -o $@ $< ../libpwdb/libpwdb.a # $(LOADLIBES)

clean:
	rm -f ${PROGS} *.o *~
