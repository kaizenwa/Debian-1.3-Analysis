EXECS = call_add1 dld-test overlay reload reload-test simple call_add2

OBJS =  hello.o print_arg.o read-a.out.o list-undefined.o get-sym.o \
	need.o test-define.o print_global.o remove.o sub.o

LIB = ../libdld.a
INCLUDE = ../dld.h
CC= gcc
CXX= g++
#	add -static to CFLAGS for Sparc Sun4.
CFLAGS = -O6 -I..
CXXFLAGS = -O6 -I..
SPECIAL = -DLIBGCC=\"`gcc --print-lib`\"

check:	all
all:	${EXECS} ${LIB}
	#				 "test simple"
	-simple
	#				 "test call_add1"
	-call_add1
	-call_add2
	#				 "test overlay"
	-overlay
	#				 "test reload"
	-reload reload-test
	#				 "test general"
	-dld-test <SAMPLE_INPUT
	#				 "tests complete"

all++:	all gxxtest
	gxxtest

clean:
	rm -f ${EXECS} gxxtest *.o *~ *.rej

${LIB}:	../dld.c $(INCLUDE)
	cd ..; make

simple:	$(LIB) $(INCLUDE) simple.c
	${CC} ${CFLAGS} simple.c $(LIB) -o simple

call_add1: $(LIB) $(INCLUDE) add1.o call_add1.c
	${CC} ${CFLAGS} call_add1.c $(LIB) -o call_add1

call_add2: $(LIB) $(INCLUDE) add2.o call_add2.c
	${CC} ${CFLAGS} call_add2.c $(LIB) -o call_add2

overlay: $(LIB) $(INCLUDE) overlay.c chain1.o chain2.o chain3.o
	${CC} ${CFLAGS} -fwritable-strings overlay.c $(LIB) -o overlay

reload: $(LIB) $(INCLUDE) reload.o reload-test
	${CC} ${CFLAGS} reload.o $(LIB) -o reload

reload-test: reload-test.o Makefile
#		Variations depending on where libc.a resides:
#	ld -r reload-test.o /lib/libc.a -o reload-test
#		For some Linuxes:
	ld -r reload-test.o /usr/lib/libc.a -o reload-test
#		For other Linuxes:
#	ld -r reload-test.o /usr/lib/libgcc.a /usr/lib/libc.a -o reload-test

hello.o: hello.c
	${CC} -c hello.c

dld-test: $(LIB) $(INCLUDE) ${OBJS} main.o
	${CC} ${CFLAGS} main.o $(LIB) -o dld-test

gxxtest: $(LIB) $(INCLUDE) ${OBJS} gxxtest.o
	${CXX} ${CXXFLAGS} gxxtest.o $(LIB) -o gxxtest

gxxtest.o: gxxtest.cc
	${CXX} -c ${CXXFLAGS} ${SPECIAL} gxxtest.cc

.c.o:
	${CC} -c ${CFLAGS} $*.c

.cc.o:
	${CXX} -c ${CXXFLAGS} $*.cc
