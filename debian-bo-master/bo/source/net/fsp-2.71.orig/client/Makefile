CLIOBJS	= lib.o lock.o util.o

all: libclient.a

libclient.a:${PARALLEL_MAKE} ${CLIOBJS}
	${RM} -f libclient.a
	${AR} ${ARFLAGS} libclient.a ${CLIOBJS}
	${RANLIB} libclient.a

clean:
	${RM} -f *.o *.a

lib.o: lib.c ../include/client_def.h ../include/common_def.h
lib.o: ../include/tweak.h ../include/proto.h
lock.o: lock.c ../include/client_def.h ../include/common_def.h
lock.o:../include/tweak.h ../include/proto.h
util.o: util.c ../include/client_def.h ../include/common_def.h
util.o: ../include/tweak.h ../include/proto.h

