COMOBJS	= random.o strdup.o udp_io.o

all: libcommon.a

libcommon.a:${PARALLEL_MAKE} ${COMOBJS}
	${RM} -f libcommon.a
	${AR} ${ARFLAGS} libcommon.a ${COMOBJS}
	${RANLIB} libcommon.a

clean:
	${RM} -f *.o *.a

random.o: random.c ../include/proto.h ../include/tweak.h
strdup.o: strdup.c ../include/proto.h ../include/tweak.h
udp_io.o: udp_io.c ../include/proto.h ../include/tweak.h
udp_io.o: ../include/common_def.h

