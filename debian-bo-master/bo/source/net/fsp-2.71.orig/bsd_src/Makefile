# This subdirectory contains modified version of files obtained from the
# uunet.uu.net archive.

# The bsd glob.c file had a bug in blkfree function in that it tries to
# free something that is actually on the stack of the glob function.
# The last call to free has been commented out to fix the bug.

BSDOBJS	= cmp.o find.o fnmatch.o function.o glob.o ls.o operator.o \
	  option.o print.o util.o

all: libbsdfsp.a

libbsdfsp.a:${PARALLEL_MAKE} ${BSDOBJS}
	${RM} -f libbsdfsp.a
	${AR} ${ARFLAGS} libbsdfsp.a ${BSDOBJS}
	${RANLIB} libbsdfsp.a

clean:
	${RM} -f *.o *.a

cmp.o: cmp.c ../include/ls.h ../include/tweak.h ../include/proto.h
find.o: find.c ../include/find.h ../include/tweak.h ../include/proto.h
fnmatch.o: fnmatch.c ../include/tweak.h ../include/proto.h
function.o: function.c ../include/find.h ../include/tweak.h ../include/proto.h
glob.o: glob.c ../include/client_def.h ../include/common_def.h
glob.o: ../include/tweak.h ../include/proto.h
ls.o: ls.c ../include/client_def.h ../include/common_def.h ../include/proto.h
ls.o: ../include/ls.h ../include/tweak.h ../include/proto.h
operator.o: operator.c ../include/find.h ../include/tweak.h ../include/proto.h
option.o: option.c ../include/find.h ../include/tweak.h ../include/proto.h
print.o: print.c ../include/ls.h ../include/tweak.h ../include/proto.h
util.o: util.c ../include/tweak.h ../include/proto.h

