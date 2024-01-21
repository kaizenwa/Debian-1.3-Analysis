CLIENTS = fcatcmd fcdcmd fducmd ffindcmd fgetcmd fgrabcmd fhostcmd flscmd \
	  fmkdir fprocmd fput frmcmd frmdircmd fver

OBJS = fcatcmd.o fcdcmd.o ffindcmd.o fgetcmd.o fgrabcmd.o fhostcmd.o \
	flscmd.o fmkdir.o fprocmd.o fput.o frmcmd.o frmdircmd.o fver.o fducmd.o

MERGE_OBJS = fcatcmd_m.o fcdcmd_m.o fducmd_m.o ffindcmd_m.o fgetcmd_m.o \
	     fgrabcmd_m.o fhostcmd_m.o flscmd_m.o fmkdir_m.o fprocmd_m.o \
	     fput_m.o frmcmd_m.o frmdircmd_m.o fver_m.o merge.o

LDEPEND	= ../common/libcommon.a ../client/libclient.a ../bsd_src/libbsdfsp.a

LIBS	= -L../common -L../bsd_src -L../client -lclient -lcommon -lbsdfsp

# We split compile and link in order not to disturb parallel make

all: compile linkclients

compile:${PARALLEL_MAKE}	${OBJS}

linkclients:${PARALLEL_MAKE}	${CLIENTS}

install: all
	for A in ${CLIENTS}; do ${INSTALL} $$A ${BIN}; done

de-install:
	for A in ${CLIENTS}; do ${RM} -f ${BIN}/$$A; done
	${RM} -f ${BIN}/fspmerge

fspmerge:${PARALLEL_MAKE} ${MERGE_OBJS} ${LDEPEND}
	${CC} -o fspmerge ${MERGE_OBJS} ${LIBS} ${EX_LIBS}

install_merge: fspmerge
	${INSTALL} fspmerge ${BIN}
	for A in ${CLIENTS}; \
	   do ${RM} -f ${BIN}/$$A; ${LN} ${BIN}/fspmerge ${BIN}/$$A; \
	done

clean:
	${RM} -f *.o ${CLIENTS}
	${RM} -f fspmerge

fcatcmd: fcatcmd.o ${LDEPEND}
	${CC} -o fcatcmd fcatcmd.o ${LIBS} ${EX_LIBS}
fcdcmd: fcdcmd.o ${LDEPEND}
	${CC} -o fcdcmd fcdcmd.o ${LIBS} ${EX_LIBS}
fducmd: fducmd.o ${LDEPEND}
	${CC} -o fducmd fducmd.o ${LIBS} ${EX_LIBS}
ffindcmd: ffindcmd.o ${LDEPEND}
	${CC} -o ffindcmd ffindcmd.o ${LIBS} ${EX_LIBS}
fgetcmd: fgetcmd.o ${LDEPEND}
	${CC} -o fgetcmd fgetcmd.o ${LIBS} ${EX_LIBS}
fgrabcmd: fgrabcmd.o ${LDEPEND}
	${CC} -o fgrabcmd fgrabcmd.o ${LIBS} ${EX_LIBS}
fhostcmd: fhostcmd.o ${LDEPEND}
	${CC} -o fhostcmd fhostcmd.o ${LIBS} ${EX_LIBS}
flscmd: flscmd.o ${LDEPEND}
	${CC} -o flscmd flscmd.o ${LIBS} ${EX_LIBS}
fmkdir: fmkdir.o ${LDEPEND}
	${CC} -o fmkdir fmkdir.o ${LIBS} ${EX_LIBS}
fprocmd: fprocmd.o ${LDEPEND}
	${CC} -o fprocmd fprocmd.o ${LIBS} ${EX_LIBS}
fput: fput.o ${LDEPEND}
	${CC} -o fput fput.o ${LIBS} ${EX_LIBS}
frmcmd: frmcmd.o ${LDEPEND}
	${CC} -o frmcmd frmcmd.o ${LIBS} ${EX_LIBS}
frmdircmd: frmdircmd.o ${LDEPEND}
	${CC} -o frmdircmd frmdircmd.o ${LIBS} ${EX_LIBS}
fver: fver.o ${LDEPEND}
	${CC} -o fver fver.o ${LIBS} ${EX_LIBS}

fcatcmd.o: fcatcmd.c ../include/client_def.h ../include/common_def.h
fcatcmd.o: ../include/tweak.h ../include/proto.h
fcdcmd.o: fcdcmd.c ../include/client_def.h ../include/common_def.h
fcdcmd.o: ../include/tweak.h ../include/proto.h
fducmd.o: fducmd.c ../include/client_def.h ../include/common_def.h
fducmd.o: ../include/tweak.h ../include/proto.h
ffindcmd.o: ffindcmd.c ../include/client_def.h ../include/common_def.h
ffindcmd.o: ../include/tweak.h ../include/proto.h
fgetcmd.o: fgetcmd.c ../include/client_def.h ../include/common_def.h
fgetcmd.o: ../include/tweak.h ../include/proto.h
fgrabcmd.o: fgrabcmd.c ../include/client_def.h ../include/common_def.h
fgrabcmd.o: ../include/tweak.h ../include/proto.h
fhostcmd.o: fhostcmd.c ../include/client_def.h ../include/common_def.h
fhostcmd.o: ../include/tweak.h ../include/proto.h
flscmd.o: flscmd.c ../include/client_def.h ../include/common_def.h
flscmd.o: ../include/tweak.h ../include/proto.h
fmkdir.o: fmkdir.c ../include/client_def.h ../include/common_def.h
fmkdir.o: ../include/tweak.h ../include/proto.h
fprocmd.o: fprocmd.c ../include/client_def.h ../include/common_def.h
fprocmd.o: ../include/tweak.h ../include/proto.h
fput.o: fput.c ../include/client_def.h ../include/common_def.h
fput.o: ../include/tweak.h ../include/proto.h
frmcmd.o: frmcmd.c ../include/client_def.h ../include/common_def.h
frmcmd.o: ../include/tweak.h ../include/proto.h
frmdircmd.o: frmdircmd.c ../include/client_def.h ../include/common_def.h
frmdircmd.o: ../include/tweak.h ../include/proto.h
fver.o: fver.c ../include/client_def.h ../include/common_def.h
fver.o: ../include/tweak.h ../include/proto.h

fcatcmd_m.o: fcatcmd.c ../include/client_def.h ../include/common_def.h
fcatcmd_m.o: ../include/tweak.h ../include/proto.h
	@echo '#include "fcatcmd.c"' > fcatcmd_m.c
	${CC} ${CFLAGS} -Dmain=fcatcmd_main -c fcatcmd_m.c
	@${RM} -f fcatcmd_m.c
fcdcmd_m.o: fcdcmd.c ../include/client_def.h ../include/common_def.h
fcdcmd_m.o: ../include/tweak.h ../include/proto.h
	@echo '#include "fcdcmd.c"' > fcdcmd_m.c
	${CC} ${CFLAGS} -Dmain=fcdcmd_main -c fcdcmd_m.c
	@${RM} -f fcdcmd_m.c
fducmd_m.o: fducmd.c ../include/client_def.h ../include/common_def.h
fducmd_m.o: ../include/tweak.h ../include/proto.h
	@echo '#include "fducmd.c"' > fducmd_m.c
	${CC} ${CFLAGS} -Dmain=fducmd_main -c fducmd_m.c
	@${RM} -f fducmd_m.c
ffindcmd_m.o: ffindcmd.c ../include/client_def.h ../include/common_def.h
ffindcmd_m.o: ../include/tweak.h ../include/proto.h
	@echo '#include "ffindcmd.c"' > ffindcmd_m.c
	${CC} ${CFLAGS} -Dmain=ffindcmd_main -c ffindcmd_m.c
	@${RM} -f ffindcmd_m.c
fgetcmd_m.o: fgetcmd.c ../include/client_def.h ../include/common_def.h
fgetcmd_m.o: ../include/tweak.h ../include/proto.h
	@echo '#include "fgetcmd.c"' > fgetcmd_m.c
	${CC} ${CFLAGS} -Dmain=fgetcmd_main -c fgetcmd_m.c
	@${RM} -f fgetcmd_m.c
fgrabcmd_m.o: fgrabcmd.c ../include/client_def.h ../include/common_def.h
fgrabcmd_m.o: ../include/tweak.h ../include/proto.h
	@echo '#include "fgrabcmd.c"' > fgrabcmd_m.c
	${CC} ${CFLAGS} -Dmain=fgrabcmd_main -c fgrabcmd_m.c
	@${RM} -f fgrabcmd_m.c
fhostcmd_m.o: fhostcmd.c ../include/client_def.h ../include/common_def.h
fhostcmd_m.o: ../include/tweak.h ../include/proto.h
	@echo '#include "fhostcmd.c"' > fhostcmd_m.c
	${CC} ${CFLAGS} -Dmain=fhostcmd_main -c fhostcmd_m.c
	@${RM} -f fhostcmd_m.c
flscmd_m.o: flscmd.c ../include/client_def.h ../include/common_def.h
flscmd_m.o: ../include/tweak.h ../include/proto.h
	@echo '#include "flscmd.c"' > flscmd_m.c
	${CC} ${CFLAGS} -Dmain=flscmd_main -c flscmd_m.c
	@${RM} -f flscmd_m.c
fmkdir_m.o: fmkdir.c ../include/client_def.h ../include/common_def.h
fmkdir_m.o: ../include/tweak.h ../include/proto.h
	@echo '#include "fmkdir.c"' > fmkdir_m.c
	${CC} ${CFLAGS} -Dmain=fmkdir_main -c fmkdir_m.c
	@${RM} -f fmkdir_m.c
fprocmd_m.o: fprocmd.c ../include/client_def.h ../include/common_def.h
fprocmd_m.o: ../include/tweak.h ../include/proto.h
	@echo '#include "fprocmd.c"' > fprocmd_m.c
	${CC} ${CFLAGS} -Dmain=fprocmd_main -c fprocmd_m.c
	@${RM} -f fprocmd_m.c
fput_m.o: fput.c ../include/client_def.h ../include/common_def.h
fput_m.o: ../include/tweak.h ../include/proto.h
	@echo '#include "fput.c"' > fput_m.c
	${CC} ${CFLAGS} -Dmain=fput_main -c fput_m.c
	@${RM} -f fput_m.c
frmcmd_m.o: frmcmd.c ../include/client_def.h ../include/common_def.h
frmcmd_m.o: ../include/tweak.h ../include/proto.h
	@echo '#include "frmcmd.c"' > frmcmd_m.c
	${CC} ${CFLAGS} -Dmain=frmcmd_main -c frmcmd_m.c
	@${RM} -f frmcmd_m.c
frmdircmd_m.o: frmdircmd.c ../include/client_def.h ../include/common_def.h
frmdircmd_m.o: ../include/tweak.h ../include/proto.h
	@echo '#include "frmdircmd.c"' > frmdircmd_m.c
	${CC} ${CFLAGS} -Dmain=frmdircmd_main -c frmdircmd_m.c
	@${RM} -f frmdircmd_m.c
fver_m.o: fver.c ../include/client_def.h ../include/common_def.h
fver_m.o: ../include/tweak.h ../include/proto.h
	@echo '#include "fver.c"' > fver_m.c
	${CC} ${CFLAGS} -Dmain=fver_main -c fver_m.c
	@${RM} -f fver_m.c
merge.o: merge.c
