# $Id: Makefile,v 1.5 1996/09/22 00:13:34 vixie Exp $

CFLAGS=	-O -I../${INCL} -I../${COMPINCL} ${DEFS} ${LOCDEFS}
LOCDEFS= -DUSE_OPTIONS_H

OBJS=	base64.o herror.o res_debug.o res_data.o \
	res_comp.o res_init.o res_mkquery.o res_query.o res_send.o \
	getnetbyaddr.o getnetbyname.o getnetent.o getnetnamadr.o \
	gethnamaddr.o sethostent.o nsap_addr.o inet_ntop.o inet_pton.o \
	inet_addr.o strerror.o

all: libresolv_pic.a

$(OBJS):
	${SHCC} ${CFLAGS} ${PIC} -c $? -o $@

clean: FRC
	rm -f errs a.out core libresolv_pic.a tags *.o *.BAK *.CKP *~ *.orig

depend: 
	@echo No dependancies in `pwd`

install:
	@echo '***  shres is *NOT* installed by make install  ***'
	@echo '*** Read shres/sunos/INSTALL for directions ***'

FRC:

libresolv_pic.a: ${OBJS}
	ar cru libresolv_pic.a ${OBJS}
	$(RANLIB) libresolv_pic.a

base64.o: ../../res/base64.c
herror.o: ../../res/herror.c
res_comp.o: ../../res/res_comp.c
res_data.o: ../../res/res_data.c
res_debug.o: ../../res/res_debug.c
res_init.o: ../../res/res_init.c
res_mkquery.o: ../../res/res_mkquery.c
res_query.o: ../../res/res_query.c
res_send.o: ../../res/res_send.c
getnetbyaddr.o: ../../res/getnetbyaddr.c
getnetbyname.o: ../../res/getnetbyname.c
getnetent.o: ../../res/getnetent.c
getnetnamadr.o: ../../res/getnetnamadr.c
gethnamaddr.o: ../../res/gethnamaddr.c
sethostent.o: ../../res/sethostent.c
nsap_addr.o: ../../res/nsap_addr.c
inet_addr.o: ../../res/inet_addr.c
inet_ntop.o: ../../res/inet_ntop.c
inet_pton.o: ../../res/inet_pton.c
strerror.o: ../../compat/lib/strerror.c
