#@(#)@(#)Makefile	2.5  2.5 4/3/91

CSRCS		=	flock.c pop_dele.c pop_dropcopy.c \
			pop_get_command.c pop_get_subcommand.c pop_init.c \
			pop_last.c pop_list.c pop_log.c pop_lower.c \
			pop_msg.c pop_parse.c pop_pass.c pop_quit.c \
			pop_rset.c pop_send.c pop_stat.c pop_updt.c \
			pop_user.c pop_xtnd.c pop_xmit.c popper.c \
			pop_bull.c xtnd_xlst.c pop_uidl.c mktemp.c \
			pop_rpop.c pop_apop.c md5.c

OBJS		=	flock.o pop_dele.o pop_dropcopy.o \
			pop_get_command.o pop_get_subcommand.o pop_init.o \
			pop_last.o pop_list.o pop_log.o pop_lower.o \
			pop_msg.o pop_parse.o pop_pass.o pop_quit.o \
			pop_rset.o pop_send.o pop_stat.o pop_updt.o \
			pop_user.o pop_xtnd.o pop_xmit.o popper.o \
			pop_bull.o xtnd_xlst.o pop_uidl.o mktemp.o \
			pop_rpop.o pop_apop.o md5.o

DOCS		=	README pop3.rfc1081 pop3e.rfc1082 popper.8

INCLUDES	=	popper.h version.h

SRCS		=	${CSRCS} ${INCLUDES}

SCCS		=	/usr/ucb/sccs

REL		=

#CC = gcc -fstrength-reduce -fpcc-struct-return -m486 -pipe -O2 -s
CC = cc

MAKEFILE	=	Makefile

#               Defines are described in the INSTALL document.

CFLAGS		=	-DBIND43 -DHAVE_VSPRINTF -DSYSV -DPOPSCO -DAUTH \
			-DAPOP=\"/etc/pop.auth\" -DPOPUID=\"pop\"

TARGET		=	popper.sco

TAR		=	${TARGET}.tar

INSTALLDIR	=	/usr/local/lib

MANPAGE		=	popper.8

CATPAGE		=	popper.0

MANDIR		=	/usr/local/man/cat8


all: ${TARGET} popauth


${TARGET}: ${OBJS}
	${CC}  ${OBJS} -o ${TARGET} -lsocket -lcrypt_d -lprot -lm -lx -lc -lndbm
#	${CC}  ${OBJS} -o ${TARGET} -lsocket -lcrypt_i -lprot -lm -lx -lc -lndbm
#	${CC}  ${OBJS} -o ${TARGET} -lsocket -lcrypt_d -lprot -lm -lx -lc_s -lndbm

popauth: popauth.o flock.o
	${CC}  -o popauth popauth.o  flock.o -lndbm

tar: ${SRCS} ${DOCS} ${MAKEFILE}
	rm -f ${TAR} *.Z*
	tar -cvf ${TAR} ${SRCS} ${DOCS} ${MAKEFILE}
	compress ${TAR}
	uuencode ${TAR}.Z ${TAR}.Z > ${TAR}.Z.uuencoded
	split -300 ${TAR}.Z.uuencoded
	mv xaa ${TAR}.Z.uuencoded.xaa
	mv xab ${TAR}.Z.uuencoded.xab
	mv xac ${TAR}.Z.uuencoded.xac
	mv xad ${TAR}.Z.uuencoded.xad
	mv xae ${TAR}.Z.uuencoded.xae

clean:
	rm -f core *.o *.Z*
	${SCCS} clean

sources: ${SRCS}

${SRCS}:
	${SCCS} get ${REL} $@ -p | expand -4 > $@

${DOCS}:
	${SCCS} get README -p | expand -4 > README
	${SCCS} get popper.8 -p | expand -4 > popper.8
	
${OBJS}:    popper.h version.h
