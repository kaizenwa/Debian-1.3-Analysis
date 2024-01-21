SEROBJS	= conf.o host.o file.o filecache.o lib.o main.o

LDEPEND	= ../common/libcommon.a

LIBS	= -L../common -lcommon

all: fspd

install: all
	${INSTALL} fspd ${BIN}

de-install:
	${RM} -f ${BIN}/fspd

fspd:${PARALLEL_MAKE}	${SEROBJS} ${LDEPEND}
	${CC} -o fspd ${SEROBJS} ${LIBS} ${EX_LIBS}

clean:
	${RM} -f *.o fspd

conf.o: conf.c ../include/server_def.h ../include/common_def.h
conf.o: ../include/tweak.h ../include/proto.h
main.o: main.c ../include/server_def.h ../include/common_def.h
main.o: ../include/tweak.h ../include/proto.h
lib.o: lib.c ../include/server_def.h ../include/common_def.h
lib.o: ../include/tweak.h ../include/proto.h
host.o: host.c ../include/server_def.h ../include/common_def.h
host.o: ../include/tweak.h ../include/proto.h
file.o: file.c ../include/server_def.h ../include/common_def.h
file.o: ../include/tweak.h ../include/proto.h
filecache.o: filecache.c ../include/server_def.h ../include/common_def.h
filecache.o: ../include/tweak.h ../include/proto.h
